import re

# מילים שמורות בקובול להתעלמות מוחלטת
COBOL_RESERVED_WORDS = {
    'SPACES', 'SPACE', 'ZERO', 'ZEROS', 'ZEROES', 'LOW-VALUES', 'HIGH-VALUES',
    'TRUE', 'FALSE', 'ON', 'OFF', 'NOT', 'AND', 'OR', 'IS', 'THAN', 'EQUAL',
    'EQUALS', 'TO', 'FROM', 'INTO', 'BY', 'GIVING', 'OTHER', 'ALL'
}

# רשימה שחורה: לעולם לא יוכרו כשם של פסקה
PARAGRAPH_EXCLUSIONS = {
    'END-IF', 'END-READ', 'END-PERFORM', 'END-EVALUATE', 'END-COMPUTE',
    'END-ADD', 'END-SUBTRACT', 'END-STRING', 'GOBACK', 'EXIT', 'CONTINUE',
    'NEXT', 'SENTENCE'
}


def extract_valid_vars(text_string):
    if not text_string: return []
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

    regex_proc_div = re.compile(r'^\s*PROCEDURE\s+DIVISION', re.IGNORECASE)
    regex_paragraph = re.compile(r'^\s*([A-Z0-9\-]+)\.\s*$', re.IGNORECASE)

    regex_move = re.compile(r'\bMOVE\s+(.+?)\s+TO\s+(.+)', re.IGNORECASE)
    regex_compute = re.compile(r'\bCOMPUTE\s+(.+?)\s*=(.+)', re.IGNORECASE)
    regex_if = re.compile(r'\bIF\s+(.+)', re.IGNORECASE)
    regex_evaluate = re.compile(r'\bWHEN\s+(.+)', re.IGNORECASE)
    regex_set = re.compile(r'\bSET\s+(.+?)\s+TO\s+(.+)', re.IGNORECASE)
    regex_add = re.compile(r'\bADD\s+(.+?)\s+TO\s+(.+)', re.IGNORECASE)
    regex_subtract = re.compile(r'\bSUBTRACT\s+(.+?)\s+FROM\s+(.+)', re.IGNORECASE)
    regex_init = re.compile(r'\bINITIALIZE\s+(.+)', re.IGNORECASE)

    # תפיסת קריאות ושליטה
    regex_perform_thru = re.compile(r'\bPERFORM\s+([A-Z0-9\-]+)\s+(?:THRU|THROUGH)\s+([A-Z0-9\-]+)', re.IGNORECASE)
    regex_perform = re.compile(r'\bPERFORM\s+([A-Z0-9\-]+)(?!\s+(?:THRU|THROUGH))', re.IGNORECASE)
    regex_call = re.compile(r'\bCALL\s+([^\s\.]+)', re.IGNORECASE)

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

        if not in_procedure_division:
            if regex_proc_div.search(line):
                in_procedure_division = True
            continue

        para_match = regex_paragraph.match(line)
        if para_match:
            candidate = para_match.group(1).upper()
            if candidate not in PARAGRAPH_EXCLUSIONS:
                current_section = candidate
                # הוספנו מקום לשמור אזהרות וקריאות חיצוניות
                sections_data[current_section] = {
                    'WRITES': set(), 'READS': set(), 'PERFORMS': set(),
                    'CALLS': set(), 'WARNINGS': set()
                }
            continue

        if current_section:
            line_no_quotes = re.sub(r"'.*?'|\".*?\"", "''", line).replace('.', ' ')

            # זיהוי תלויות נתונים
            match = regex_move.search(line_no_quotes)
            if match:
                src, dest = match.groups()
                sections_data[current_section]['READS'].update(extract_valid_vars(src))
                sections_data[current_section]['WRITES'].update(extract_valid_vars(dest))

            match = regex_compute.search(line_no_quotes)
            if match:
                dest, src = match.groups()
                sections_data[current_section]['WRITES'].update(extract_valid_vars(dest))
                sections_data[current_section]['READS'].update(extract_valid_vars(src))

            match = regex_set.search(line_no_quotes)
            if match:
                dest, _ = match.groups()
                sections_data[current_section]['WRITES'].update(extract_valid_vars(dest))

            match = regex_add.search(line_no_quotes)
            if match:
                src, dest = match.groups()
                sections_data[current_section]['READS'].update(extract_valid_vars(src))
                dest_vars = extract_valid_vars(dest)
                sections_data[current_section]['READS'].update(dest_vars)
                sections_data[current_section]['WRITES'].update(dest_vars)

            match = regex_subtract.search(line_no_quotes)
            if match:
                src, dest = match.groups()
                sections_data[current_section]['READS'].update(extract_valid_vars(src))
                dest_vars = extract_valid_vars(dest)
                sections_data[current_section]['READS'].update(dest_vars)
                sections_data[current_section]['WRITES'].update(dest_vars)

            match = regex_init.search(line_no_quotes)
            if match:
                sections_data[current_section]['WRITES'].update(extract_valid_vars(match.group(1)))

            for regex_cond in [regex_if, regex_evaluate]:
                match = regex_cond.search(line_no_quotes)
                if match:
                    sections_data[current_section]['READS'].update(extract_valid_vars(match.group(1)))

            # זיהוי תלויות שליטה (Control Flow & Calls)
            match_thru = regex_perform_thru.search(line_no_quotes)
            if match_thru:
                sec1, sec2 = match_thru.groups()
                sections_data[current_section]['PERFORMS'].add(sec1.upper())
                sections_data[current_section]['WARNINGS'].add(f"Contains 'PERFORM THRU' ({sec1} THRU {sec2})")
            else:
                match_perf = regex_perform.search(line_no_quotes)
                if match_perf:
                    sections_data[current_section]['PERFORMS'].add(match_perf.group(1).upper())

            match_call = regex_call.search(line_no_quotes)
            if match_call:
                prog_name = match_call.group(1).replace("'", "").replace('"', '')
                sections_data[current_section]['CALLS'].add(prog_name.upper())

    # המרה ל-List לשמירה נקייה
    for sec in sections_data:
        sections_data[sec] = {k: list(v) for k, v in sections_data[sec].items()}

    return sections_data


# ==========================================
# חלק 2: מנתח התלויות וה-Call Graph
# ==========================================
def find_sections_writing_to(data, target_var):
    return [sec for sec, sec_data in data.items() if target_var in sec_data['WRITES']]


def build_tree_and_collect(data, target_var, required_vars, data_sections, visited_sections, depth=0):
    if target_var in required_vars:
        return f"{'  ' * depth}└─ [VAR]  {target_var} (Already traced)\n"

    required_vars.add(target_var)
    tree_output = f"{'  ' * depth}■ [VAR]  Target: {target_var}\n"

    writers = find_sections_writing_to(data, target_var)
    if not writers:
        return tree_output + f"{'  ' * (depth + 1)}└─ [External Input / Master DB]\n"

    for writer in writers:
        tree_output += f"{'  ' * (depth + 1)}├─ [DATA] Populated by: {writer}\n"
        data_sections.add(writer)

        if writer not in visited_sections:
            visited_sections.add(writer)

            reads = data[writer]['READS']
            for read_var in reads:
                tree_output += f"{'  ' * (depth + 2)}├─ [VAR]  Requires: {read_var}\n"
                tree_output += build_tree_and_collect(data, read_var, required_vars, data_sections, visited_sections,
                                                      depth + 3)

            performs = data[writer]['PERFORMS']
            for perf in performs:
                tree_output += f"{'  ' * (depth + 2)}├─ [CTRL] Performs Helper: {perf}\n"
                if perf not in visited_sections:
                    visited_sections.add(perf)
                    # Helper sections are visited, we check what variables they read
                    for helper_read in data.get(perf, {}).get('READS', []):
                        tree_output += build_tree_and_collect(data, helper_read, required_vars, data_sections,
                                                              visited_sections, depth + 3)

    return tree_output


def resolve_control_flow(cobol_data, all_required_sections):
    """מוצא מי קרא לסקשנים שגילינו שאנחנו צריכים (כמו MAIN)"""
    added_new = True
    while added_new:
        added_new = False
        for caller_sec, data in cobol_data.items():
            if caller_sec in all_required_sections:
                continue
            for perf in data['PERFORMS']:
                if perf in all_required_sections:
                    all_required_sections.add(caller_sec)
                    added_new = True
                    break
    return all_required_sections


def generate_action_report(cobol_data, data_sections, all_required_sections):
    all_secs_set = set(cobol_data.keys())

    # מיון לשלושת הדליים
    delete_secs = all_secs_set - all_required_sections

    keep_secs = set()
    investigate_secs = set()

    # חלוקה בין KEEP ל-INVESTIGATE
    for sec in all_required_sections:
        is_data_writer = sec in data_sections
        has_warnings = len(cobol_data[sec]['WARNINGS']) > 0
        has_calls = len(cobol_data[sec]['CALLS']) > 0

        # אם אין לו אזהרות, והוא כותב מידע נדרש -> KEEP
        if is_data_writer and not has_warnings and not has_calls:
            keep_secs.add(sec)
        else:
            # אם הוא רק Control flow, או שיש לו אזהרות -> INVESTIGATE
            investigate_secs.add(sec)

    report = "\n" + "=" * 70 + "\n"
    report += "           ACTION REPORT: KEEP vs. INVESTIGATE vs. DELETE\n"
    report += "=" * 70 + "\n\n"

    report += f"🟢 SECTIONS TO KEEP (Data Logic - Total: {len(keep_secs)}):\n"
    report += "-" * 50 + "\n"
    for sec in sorted(list(keep_secs)):
        report += f"  [ KEEP ] {sec}\n"

    report += f"\n🟡 SECTIONS TO INVESTIGATE (Control / Warnings - Total: {len(investigate_secs)}):\n"
    report += "-" * 50 + "\n"
    for sec in sorted(list(investigate_secs)):
        notes = []
        if sec not in data_sections: notes.append("Control Flow Only")
        if cobol_data[sec]['CALLS']: notes.append(f"CALLS: {', '.join(cobol_data[sec]['CALLS'])}")
        if cobol_data[sec]['WARNINGS']: notes.extend(cobol_data[sec]['WARNINGS'])

        note_str = " | ".join(notes)
        report += f"  [ CHECK ] {sec:<30} -> WHY? {note_str}\n"

    report += f"\n🔴 SECTIONS TO DELETE (Dead Code - Total: {len(delete_secs)}):\n"
    report += "-" * 50 + "\n"
    for sec in sorted(list(delete_secs)):
        report += f"  [ DELETE ] {sec}\n"

    return report


# ==========================================
# הרצה ראשית
# ==========================================
if __name__ == "__main__":
    cobol_file_path = 'big_cobol_file.cob'
    target_variable = 'RL-NOTES'

    print(">> Parsing COBOL File...")
    cobol_data = parse_cobol_file(cobol_file_path)
    print(f">> Successfully mapped {len(cobol_data)} Procedure Paragraphs.\n")

    print(f">> Building Dependency Tree for: {target_variable}")
    required_vars = set()
    data_sections = set()  # סקשנים שמעדכנים נתונים בפועל
    visited_sections = set()  # כל הסקשנים שהעץ דרך בהם

    tree_text = build_tree_and_collect(cobol_data, target_variable, required_vars, data_sections, visited_sections)

    # השלמת ה-Control Flow (כדי למצוא את MAIN ודומיו)
    all_required_sections = resolve_control_flow(cobol_data, visited_sections.copy())

    # יצירת הדוח המחולק ל-3
    action_report = generate_action_report(cobol_data, data_sections, all_required_sections)

    final_output = tree_text + action_report

    output_filename = 'COBOL_SLICE_REPORT.txt'
    with open(output_filename, 'w', encoding='utf-8') as out_f:
        out_f.write(final_output)

    print(final_output)
    print(f"\n>> DONE! The full report is saved to: {output_filename}")