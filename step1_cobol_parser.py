import re


# ==========================================
# חלק 1: מנתח הקובול (Parser)
# ==========================================
def parse_cobol_file(file_path):
    sections_data = {}
    current_section = None
    in_procedure_division = False

    # ביטויים רגולריים
    regex_proc_div = re.compile(r'^PROCEDURE\s+DIVISION\.', re.IGNORECASE)
    regex_paragraph = re.compile(r'^([A-Z0-9\-]+)\.$', re.IGNORECASE)
    regex_move = re.compile(r'MOVE\s+(.+?)\s+TO\s+(.+)', re.IGNORECASE)
    regex_compute = re.compile(r'COMPUTE\s+(.+?)\s*=(.+)', re.IGNORECASE)
    regex_if = re.compile(r'IF\s+(.+?)\s+(?:=|>|<|EQUAL|NOT)', re.IGNORECASE)
    regex_evaluate = re.compile(r'WHEN\s+(.+)', re.IGNORECASE)
    regex_perform = re.compile(r'PERFORM\s+([A-Z0-9\-]+)', re.IGNORECASE)

    # קריאת הקובץ עם תמיכה בעברית
    try:
        with open(file_path, 'r', encoding='cp1255', errors='replace') as f:
            lines = f.readlines()
    except UnicodeDecodeError:
        with open(file_path, 'r', encoding='utf-8', errors='replace') as f:
            lines = f.readlines()

    for line in lines:
        line = line.strip()
        if not line or line.startswith('*'):
            continue

        # הגעה לחלק הלוגי
        if not in_procedure_division:
            if regex_proc_div.match(line):
                in_procedure_division = True
            continue

        # זיהוי פסקה חדשה
        para_match = regex_paragraph.match(line)
        if para_match:
            current_section = para_match.group(1).upper()
            sections_data[current_section] = {'WRITES': set(), 'READS': set(), 'PERFORMS': set()}
            continue

        if current_section:
            # מחיקת טקסט בתוך גרשיים כדי למנוע זיהוי שגוי של משתנים
            line_no_quotes = re.sub(r"'.*?'|\".*?\"", "''", line)

            # זיהוי MOVE
            move_match = regex_move.search(line_no_quotes)
            if move_match:
                sources_str, dests_str = move_match.groups()
                for dest in dests_str.replace('.', '').split():
                    dest_clean = dest.strip().upper()
                    if dest_clean not in ['SPACES', 'ZERO', 'ZEROS']:
                        sections_data[current_section]['WRITES'].add(dest_clean)

                for source in re.findall(r'[A-Z0-9\-]+', sources_str):
                    source_clean = source.upper()
                    if not source_clean.isnumeric() and source_clean not in ['SPACES', 'ZERO', 'ZEROS']:
                        sections_data[current_section]['READS'].add(source_clean)

            # זיהוי COMPUTE
            compute_match = regex_compute.search(line_no_quotes)
            if compute_match:
                dest, sources = compute_match.groups()
                sections_data[current_section]['WRITES'].add(dest.strip().upper())
                for var in re.findall(r'[A-Z0-9\-]+', sources.upper()):
                    if not var.isnumeric() and var not in ['SPACES', 'ZERO']:
                        sections_data[current_section]['READS'].add(var)

            # זיהוי IF
            if_match = regex_if.search(line_no_quotes)
            if if_match:
                for var in re.findall(r'[A-Z0-9\-]+', if_match.group(1).upper()):
                    if not var.isnumeric() and var not in ['NOT']:
                        sections_data[current_section]['READS'].add(var)

            # זיהוי EVALUATE
            when_match = regex_evaluate.search(line_no_quotes)
            if when_match:
                for var in re.findall(r'[A-Z0-9\-]+', when_match.group(1).upper()):
                    if not var.isnumeric() and var not in ['OTHER', 'TRUE', 'FALSE']:
                        sections_data[current_section]['READS'].add(var)

            # זיהוי PERFORM
            perf_match = regex_perform.search(line_no_quotes)
            if perf_match:
                sections_data[current_section]['PERFORMS'].add(perf_match.group(1).upper())

    # המרה לרשימות רגילות
    for sec in sections_data:
        sections_data[sec] = {k: list(v) for k, v in sections_data[sec].items()}

    return sections_data


# ==========================================
# חלק 2: בונה עץ התלויות (Tree Builder)
# ==========================================
def find_sections_writing_to(data, target_var):
    """מוצא אילו פסקאות מעדכנות משתנה מסוים"""
    writers = []
    for sec_name, sec_data in data.items():
        if target_var in sec_data['WRITES']:
            writers.append(sec_name)
    return writers


def build_dependency_tree(data, target_var, visited_vars=None, visited_sections=None, depth=0):
    """בונה עץ רקורסיבי של תלויות משתנים ופסקאות"""
    if visited_vars is None: visited_vars = set()
    if visited_sections is None: visited_sections = set()

    # מניעת לולאה אינסופית
    if target_var in visited_vars:
        return f"{'  ' * depth}└─ [Variable: {target_var}] (Already traced)\n"

    visited_vars.add(target_var)
    tree_output = f"{'  ' * depth}■ Target Variable: {target_var}\n"

    writers = find_sections_writing_to(data, target_var)
    if not writers:
        return tree_output + f"{'  ' * (depth + 1)}└─ [External Input / Unidentified]\n"

    for writer in writers:
        tree_output += f"{'  ' * (depth + 1)}├─ Populated by Section: {writer}\n"

        if writer not in visited_sections:
            visited_sections.add(writer)

            # בדיקת משתנים שהפסקה הזו קוראת
            reads = data[writer]['READS']
            for read_var in reads:
                tree_output += f"{'  ' * (depth + 2)}├─ Requires Flag/Var: {read_var}\n"
                tree_output += build_dependency_tree(data, read_var, visited_vars.copy(), visited_sections.copy(),
                                                     depth + 3)

            # בדיקת פסקאות עזר שהפסקה הזו מפעילה
            performs = data[writer]['PERFORMS']
            for perf in performs:
                tree_output += f"{'  ' * (depth + 2)}├─ Performs Helper Section: {perf}\n"
                if perf not in visited_sections:
                    visited_sections.add(perf)
                    for helper_read in data.get(perf, {}).get('READS', []):
                        tree_output += build_dependency_tree(data, helper_read, visited_vars.copy(),
                                                             visited_sections.copy(), depth + 3)

    return tree_output


# ==========================================
# הרצה ראשית
# ==========================================
if __name__ == "__main__":
    # 1. הגדר כאן את הנתיב לקובץ הקובול שלך
    cobol_file_path = 'big_cobol_file.cob'

    # 2. הגדר כאן את המשתנה שאתה מחפש (אותיות גדולות)
    target_variable = 'RL-NOTES'

    print(f"1. מתחיל ניתוח של הקובץ: {cobol_file_path}")
    cobol_data = parse_cobol_file(cobol_file_path)
    print(f"   נמצאו {len(cobol_data)} פסקאות לוגיות.\n")

    print(f"2. בונה עץ תלויות עבור המשתנה: {target_variable}")
    final_tree = build_dependency_tree(cobol_data, target_variable)

    # שמירת הפלט לקובץ טקסט שיהיה נוח לקרוא
    output_filename = 'dependency_tree_output.txt'
    with open(output_filename, 'w', encoding='utf-8') as out_f:
        out_f.write(final_tree)

    print(final_tree)
    print(f"\nהתהליך הסתיים בהצלחה! העץ המלא נשמר גם לקובץ: {output_filename}")