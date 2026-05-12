import json


def load_parsed_data(json_file):
    with open(json_file, 'r', encoding='utf-8') as f:
        return json.load(f)


def find_sections_writing_to(data, target_var):
    """מוצא אילו סקשנים מעדכנים משתנה מסוים"""
    writers = []
    for sec_name, sec_data in data.items():
        if target_var in sec_data['WRITES']:
            writers.append(sec_name)
    return writers


def build_dependency_tree(data, target_var, visited_vars=None, visited_sections=None, depth=0):
    if visited_vars is None: visited_vars = set()
    if visited_sections is None: visited_sections = set()

    # מניעת לולאה אינסופית (Circular Dependency)
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

            # בדיקת משתנים שהסקשן הזה קורא
            reads = data[writer]['READS']
            for read_var in reads:
                tree_output += f"{'  ' * (depth + 2)}├─ Requires Flag/Var: {read_var}\n"
                # קריאה רקורסיבית למשתנה החדש (למשל אחד מ-7 הפלאגים)
                tree_output += build_dependency_tree(data, read_var, visited_vars.copy(), visited_sections.copy(),
                                                     depth + 3)

            # בדיקת סקשני עזר שהסקשן הזה מפעיל
            performs = data[writer]['PERFORMS']
            for perf in performs:
                tree_output += f"{'  ' * (depth + 2)}├─ Performs Helper Section: {perf}\n"
                if perf not in visited_sections:
                    # נוסיף לוגיקה לעקוב מה קורה בתוך סקשן העזר
                    visited_sections.add(perf)
                    for helper_read in data.get(perf, {}).get('READS', []):
                        tree_output += build_dependency_tree(data, helper_read, visited_vars.copy(),
                                                             visited_sections.copy(), depth + 3)

    return tree_output


if __name__ == "__main__":
    cobol_data = load_parsed_data('parsed_cobol.json')
    target = "PZ_TRN_TXT_500"  # הגדרת שמות המשתנים באותיות גדולות

    print(f"--- Starting trace for: {target} ---")
    tree = build_dependency_tree(cobol_data, target)

    # שמירת העץ לקובץ כדי שתוכל לקרוא בנחת
    with open('dependency_tree.txt', 'w', encoding='utf-8') as out_f:
        out_f.write(tree)

    print(tree)
    print("Tree trace complete. Check dependency_tree.txt")