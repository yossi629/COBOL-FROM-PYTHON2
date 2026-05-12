import re
import json

def parse_cobol_file(file_path, output_json):
    # מילון שיחזיק את כל הסקשנים
    sections_data = {}
    current_section = None


    # ביטויים רגולריים בסיסיים לזיהוי פעולות (דורש התאמה לסינטקס המדויק שלך)
    # נחפש מילים באותיות גדולות כמו שביקשת בעבר עבור שדות
    regex_section = re.compile(r'^([A-Z0-9\-]+)\s+SECTION\.', re.IGNORECASE)
    regex_move = re.compile(r'MOVE\s+(.+?)\s+TO\s+(.+)', re.IGNORECASE)
    regex_compute = re.compile(r'COMPUTE\s+(.+?)\s*=(.+)', re.IGNORECASE)
    regex_if = re.compile(r'IF\s+(.+?)\s+(?:=|>|<|EQUAL)', re.IGNORECASE)
    regex_perform = re.compile(r'PERFORM\s+([A-Z0-9\-]+)', re.IGNORECASE)

    # קריאת הקובץ (תמיכה בעברית)
    try:
        with open(file_path, 'r', encoding='cp1255', errors='replace') as f:
            lines = f.readlines()
    except UnicodeDecodeError:
        with open(file_path, 'r', encoding='utf-8', errors='replace') as f:
            lines = f.readlines()

    for line in lines:
        line = line.strip()
        if not line or line.startswith('*'): # התעלמות מהערות
            continue

        # זיהוי תחילת סקשן
        sec_match = regex_section.match(line)
        if sec_match:
            current_section = sec_match.group(1).upper()
            sections_data[current_section] = {'WRITES': set(), 'READS': set(), 'PERFORMS': set()}
            continue

        if current_section:
            # זיהוי MOVE (כתיבה למשתנה)
            move_match = regex_move.search(line)
            if move_match:
                source, dest = move_match.groups()
                sections_data[current_section]['READS'].add(source.strip().upper())
                # ננקה פסיקים ונקודות בסוף הפקודה
                sections_data[current_section]['WRITES'].add(dest.strip().replace('.', '').upper())

            # זיהוי COMPUTE
            compute_match = regex_compute.search(line)
            if compute_match:
                dest, sources = compute_match.groups()
                sections_data[current_section]['WRITES'].add(dest.strip().upper())
                # חילוץ כל המשתנים בצד ימין של המשוואה
                for var in re.findall(r'[A-Z0-9\-]+', sources.upper()):
                    if not var.isnumeric():
                        sections_data[current_section]['READS'].add(var)

            # זיהוי IF (קריאת משתנה)
            if_match = regex_if.search(line)
            if if_match:
                var = if_match.group(1).strip().upper()
                if not var.isnumeric():
                    sections_data[current_section]['READS'].add(var)

            # זיהוי קריאה לסקשני עזר
            perf_match = regex_perform.search(line)
            if perf_match:
                sections_data[current_section]['PERFORMS'].add(perf_match.group(1).upper())

    # המרת Set ל-List כדי שנוכל לשמור כ-JSON
    for sec in sections_data:
        sections_data[sec] = {k: list(v) for k, v in sections_data[sec].items()}

    with open(output_json, 'w', encoding='utf-8') as f:
        json.dump(sections_data, f, indent=4)
    print(f"Parsed {len(sections_data)} sections. Data saved to {output_json}")

if __name__ == "__main__":
    parse_cobol_file('BIG_COBOL_PROG.cbl', 'parsed_cobol.json')