import re

# מילים שמורות בקובול שנתעלם מהן כדי לנקות את העץ
COBOL_RESERVED_WORDS = {
    'SPACES', 'SPACE', 'ZERO', 'ZEROS', 'ZEROES', 'LOW-VALUES', 'HIGH-VALUES',
    'TRUE', 'FALSE', 'ON', 'OFF', 'NOT', 'AND', 'OR', 'IS', 'THAN', 'EQUAL',
    'EQUALS', 'TO', 'FROM', 'INTO', 'BY', 'GIVING'
}


def extract_valid_vars(text_string):
    """פונקציית עזר לזיהוי משתנים אמיתיים והתעלמות ממספרים ומילים שמורות"""
    if not text_string: return []
    # חילוץ מילים חוקיות בקובול (אותיות, מספרים ומקפים)
    words = re.findall(r'[A-Z0-9\-]+', text_string.upper())
    valid_vars = []
    for w in words:
        if not w.isnumeric() and w not in COBOL_RESERVED_WORDS:
            valid_vars.append(w)
    return valid_vars


# ==========================================
# חלק 1: מנתח הקובול (Parser)
# ==========================================
def parse_cobol_file(file_path):
    sections_data = {}
    current_section = None
    in_procedure_division = False

    # ביטויים רגולריים גמישים יותר שמתעלמים מרווחים מיותרים
    regex_proc_div = re.compile(r'^\s*PROCEDURE\s+DIVISION', re.IGNORECASE)
    regex_paragraph = re.compile(r'^\s*([A-Z0-9\-]+)\.\s*$', re.IGNORECASE)

    # פקודות קובול
    regex_move = re.compile(r'\bMOVE\s+(.+?)\s+TO\s+(.+)', re.IGNORECASE)
    regex_compute = re.compile(r'\bCOMPUTE\s+(.+?)\s*=(.+)', re.IGNORECASE)
    regex_if = re.compile(r'\bIF\s+(.+)', re.IGNORECASE)
    regex_evaluate = re.compile(r'\bWHEN\s+(.+)', re.IGNORECASE)
    regex_perform = re.compile(r'\bPERFORM\s+([A-Z0-9\-]+)', re.IGNORECASE)
    regex_set = re.compile(r'\bSET\s+(.+?)\s+TO\s+(.+)', re.IGNORECASE)
    regex_add = re.compile(r'\bADD\s+(.+?)\s+TO\s+(.+)', re.IGNORECASE)
    regex_subtract = re.compile(r'\bSUBTRACT\s+(.+?)\s+FROM\s+(.+)', re.IGNORECASE)
    regex_init = re.compile(r'\bINITIALIZE\s+(.+)', re.IGNORECASE)

    try:
        with open(file_path, 'r', encoding='cp1255', errors='replace') as f:
            lines = f.readlines()
    except UnicodeDecodeError:
        with open(file_path, 'r', encoding='utf-8', errors='replace') as f:
            lines = f.readlines()

    for line in lines:
        line = line.strip()
        # התעלמות מהערות קובול (כוכבית בטור 7 מומרת לתחילת שורה ב-strip)
        if not line or line.startswith('*'):
            continue

        if not in_procedure_division:
            if regex_proc_div.search(line):
                in_procedure_division = True
            continue

        # זיהוי פסקה חדשה
        para_match = regex_paragraph.match(line)
        if para_match:
            current_section = para_match.group(1).upper()
            sections_data[current_section] = {'WRITES': set(), 'READS': set(), 'PERFORMS': set()}
            continue

        if current_section:
            # מחיקת טקסט בתוך גרשיים
            line_no_quotes = re.sub(r"'.*?'|\".*?\"", "''", line).replace('.', ' ')

            # זיהוי MOVE
            match = regex_move.search(line_no_quotes)
            if match:
                src, dest = match.groups()
                sections_data[current_section]['READS'].update(extract_valid_vars(src))
                sections_data[current_section]['WRITES'].update(extract_valid_vars(dest))

            # זיהוי COMPUTE
            match = regex_compute.search(line_no_quotes)
            if match:
                dest, src = match.groups()
                sections_data[current_section]['WRITES'].update(extract_valid_vars(dest))
                sections_data[current_section]['READS'].update(extract_valid_vars(src))

            # זיהוי SET (עבור פלאגים של Level 88)
            match = regex_set.search(line_no_quotes)
            if match:
                dest, _ = match.groups()  # הערך לרוב הוא TRUE/FALSE, אז מתעלמים ממנו
                sections_data[current_section]['WRITES'].update(extract_valid_vars(dest))

            # זיהוי ADD
            match = regex_add.search(line_no_quotes)
            if match:
                src, dest = match.groups()
                sections_data[current_section]['READS'].update(extract_valid_vars(src))
                # ADD מעדכן את היעד, ולכן היעד גם נקרא וגם נכתב
                dest_vars = extract_valid_vars(dest)
                sections_data[current_section]['READS'].update(dest_vars)
                sections_data[current_section]['WRITES'].update(dest_vars)

            # זיהוי SUBTRACT
            match = regex_subtract.search(line_no_quotes)
            if match:
                src, dest = match.groups()
                sections_data[current_section]['READS'].update(extract_valid_vars(src))
                dest_vars = extract_valid_vars(dest)
                sections_data[current_section]['READS'].update(dest_vars)
                sections_data[current_section]['WRITES'].update(dest_vars)

            # זיהוי INITIALIZE
            match = regex_init.search(line_no_quotes)
            if match:
                sections_data[current_section]['WRITES'].update(extract_valid_vars(match.group(1)))

            # זיהוי IF ו- WHEN
            for regex_cond in [regex_if, regex_evaluate]:
                match = regex_cond.search(line_no_quotes)
                if match:
                    sections_data[current_section]['READS'].update(extract_valid_vars(match.group(1)))

            # זיהוי קריאות (PERFORM)
            match = regex_perform.search(line_no_quotes)
            if match:
                sections_data[current_section]['PERFORMS'].add(match.group(1).upper())

    # המרה ל-List לשמירה נקייה
    for sec in sections_data:
        sections_data[sec] = {k: list(v) for k, v in sections_data[sec].items()}

    return sections_data


# ==========================================
# חלק 2: מנתח התלויות ובניית הדו"ח
# ==========================================
def find_sections_writing_to(data, target_var):
    return [sec for sec, sec_data in data.items() if target_var in sec_data['WRITES']]


def build_tree_and_collect_sections(data, target_var, visited_vars=None, visited_sections=None, depth=0):
    """בונה את העץ ובמקביל אוסף את הפסקאות ההכרחיות"""
    if visited_vars is None: visited_vars = set()
    if visited_sections is None: visited_sections = set()

    if target_var in visited_vars:
        return f"{'  ' * depth}└─ [Variable: {target_var}] (Already traced)\n"

    visited_vars.add(target_var)
    tree_output = f"{'  ' * depth}■ Target Variable: {target_var}\n"

    writers = find_sections_writing_to(data, target_var)
    if not writers:
        return tree_output + f"{'  ' * (depth + 1)}└─ [External Input / Master DB]\n"

    for writer in writers:
        tree_output += f"{'  ' * (depth + 1)}├─ Populated by Section: {writer}\n"

        if writer not in visited_sections:
            visited_sections.add(writer)

            reads = data[writer]['READS']
            for read_var in reads:
                tree_output += f"{'  ' * (depth + 2)}├─ Requires Flag/Var: {read_var}\n"
                tree_output += build_tree_and_collect_sections(data, read_var, visited_vars, visited_sections,
                                                               depth + 3)

            performs = data[writer]['PERFORMS']
            for perf in performs:
                tree_output += f"{'  ' * (depth + 2)}├─ Performs Helper Section: {perf}\n"
                if perf not in visited_sections:
                    visited_sections.add(perf)
                    for helper_read in data.get(perf, {}).get('READS', []):
                        tree_output += build_tree_and_collect_sections(data, helper_read, visited_vars,
                                                                       visited_sections, depth + 3)

    return tree_output


def generate_action_report(all_sections, required_sections):
    """מייצר את טבלאות ה-KEEP ו-DELETE"""
    all_secs_set = set(all_sections.keys())
    required_secs_set = set(required_sections)
    delete_secs_set = all_secs_set - required_secs_set

    report = "\n" + "=" * 60 + "\n"
    report += "                ACTION REPORT: KEEP VS. DELETE\n"
    report += "=" * 60 + "\n\n"

    report += f"✅ SECTIONS TO KEEP (Total: {len(required_secs_set)}):\n"
    report += "-" * 30 + "\n"
    for sec in sorted(list(required_secs_set)):
        report += f"  [ KEEP ] {sec}\n"

    report += f"\n❌ SECTIONS TO DELETE (Total: {len(delete_secs_set)}):\n"
    report += "-" * 30 + "\n"
    for sec in sorted(list(delete_secs_set)):
        report += f"  [ DELETE ] {sec}\n"

    return report


# ==========================================
# הרצה ראשית
# ==========================================
if __name__ == "__main__":
    # 1. הנתיב לקובץ הקובול (שים את הקובץ של ה-80,000 רשומות כאן)
    cobol_file_path = 'big_cobol_file.cob'

    # 2. המשתנה המטרה שתרצה לחשב
    target_variable = 'RL-NOTES'

    print(">> Parsing COBOL File... (This might take a few seconds for 80K lines)")
    cobol_data = parse_cobol_file(cobol_file_path)
    print(f">> Successfully mapped {len(cobol_data)} Procedure Paragraphs.\n")

    print(f">> Building Dependency Tree for: {target_variable}")
    # אנחנו מזינים קבוצה ריקה שתתמלא בפסקאות שניצלנו במהלך סריקת העץ
    used_sections = set()
    tree_text = build_tree_and_collect_sections(cobol_data, target_variable, visited_sections=used_sections)

    # יצירת הדו"ח הסופי
    action_report = generate_action_report(cobol_data, used_sections)

    final_output = tree_text + action_report

    output_filename = 'COBOL_SLICE_REPORT.txt'
    with open(output_filename, 'w', encoding='utf-8') as out_f:
        out_f.write(final_output)

    print(final_output)
    print(f"\n>> DONE! The full report is saved to: {output_filename}")