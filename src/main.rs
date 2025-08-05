use std::collections::HashMap;
use std::env;
use std::fmt;
use std::fs;
use std::fs::File;
use std::io::Write;

// "an1.1-10" is considered as SectionRange(1,10)
// "sn1.55" is considered as Section(55)
#[derive(Debug, Clone)]
enum Section {
    Section(u32),
    SectionRange(u32, u32),
}

impl fmt::Display for Section {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Section::Section(v) => write!(f, "{v}"),
            Section::SectionRange(start, end) => write!(f, "{start}-{end}"),
        }
    }
}

// For "sn1.55" 1 is considered as the discourse and 55 as segment
#[derive(Debug, Clone)]
struct Sutta {
    discourse: Option<u32>,
    section: Option<Section>,
}

impl fmt::Display for Sutta {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(discourse) = self.discourse {
            write!(f, "{discourse}").expect("write err");
        }
        if let Some(section) = self.section.clone() {
            write!(f, ".{section}").expect("write err");
        }

        Ok(())
    }
}

impl Eq for Sutta {}

impl PartialEq for Sutta {
    fn eq(&self, _: &Self) -> bool {
        todo!()
    }
}

impl PartialOrd for Sutta {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Sutta {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.discourse.cmp(&other.discourse)
    }
}

#[derive(Debug)]
struct Node {
    uid: String,
    sutta: Sutta,
    children: Vec<usize>,
}

struct Tree {
    arena: Vec<Node>,
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} C({}):{}",
            self.uid,
            self.sutta,
            self.children.len(),
            self.children
                .iter()
                .map(|&num| num.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

// Given a uid, returns the index at which the section number starts from. This
// can be used to strip the sutta id prefix from the uid
// Eg: For "sn1.55" it will return the value 2 where the section number "1" is
fn get_idx_from_uid(uid: &str) -> usize {
    if uid.starts_with("snp") || uid.starts_with("dhp") || uid.starts_with("iti") {
        3
    } else if uid.starts_with("dn")
        || uid.starts_with("mn")
        || uid.starts_with("pv")
        || uid.starts_with("cp")
        || uid.starts_with("ja")
        || uid.starts_with("sn")
        || uid.starts_with("an")
        || uid.starts_with("kp")
        || uid.starts_with("ud")
    {
        2
    } else if uid.starts_with("thag") || uid.starts_with("thig") {
        4
    } else {
        panic!("unknown UID: {uid}")
    }
}

// Converts a uid to an instance of `Sutta` struct
fn extract_sutta_info(uid: &str) -> Sutta {
    let idx = get_idx_from_uid(uid);

    // These dont have a section
    // Eg: "mn91", "dn26", "ja293"
    if uid.starts_with("dn")
        || uid.starts_with("mn")
        || uid.starts_with("iti")
        || uid.starts_with("pv")
        || uid.starts_with("cp")
        || uid.starts_with("ja")
    {
        return Sutta {
            discourse: Some(uid[idx..].parse::<u32>().unwrap()),
            section: None,
        };
    }

    let dot_parts = uid.split('.').collect::<Vec<&str>>();

    assert!(dot_parts.len() == 2);
    assert!(dot_parts[0].len() > 2);

    let discourse = dot_parts[0][idx..].parse::<u32>().unwrap();

    let hyphen_parts = dot_parts[1].split('-').collect::<Vec<&str>>();

    let section = match hyphen_parts.len() {
        2 => Some(Section::SectionRange(
            hyphen_parts[0].parse::<u32>().unwrap(),
            hyphen_parts[1].parse::<u32>().unwrap(),
        )),
        1 => Some(Section::Section(dot_parts[1].parse::<u32>().unwrap())),
        _ => {
            panic!("unsupported: {} {}", uid, hyphen_parts.len())
        }
    };

    Sutta {
        discourse: Some(discourse),
        section,
    }
}

fn generate_uid(sutta: &Sutta, uid: &str) -> String {
    let idx = get_idx_from_uid(uid);
    if uid.starts_with("dn")
        || uid.starts_with("mn")
        || uid.starts_with("iti")
        || uid.starts_with("pv")
        || uid.starts_with("cp")
        || uid.starts_with("ja")
    {
        return format!("{}{}", &uid[..idx], &sutta.discourse.unwrap().to_string());
    }
    let suffix = match sutta.section.clone().unwrap() {
        Section::Section(x) => format!("{x}"),
        Section::SectionRange(a, b) => format!("{a}-{b}"),
    };
    format!(
        "{}{}.{}",
        &uid[..idx],
        &sutta.discourse.unwrap().to_string(),
        suffix
    )
}

// Used for parsing the jsons in "structure/tree/sutta/". Since the
// json is deeply nested, we use recursion to parse it
fn get_list_of_children(
    json_obj: &serde_json::Value,
    tree: &mut Tree,
    #[allow(clippy::used_underscore_binding)] _recursion_level: u32,
) -> Vec<usize> {
    if json_obj.is_array() {
        let mut children_list: Vec<usize> = Vec::new();
        for i in json_obj.as_array().unwrap() {
            if i.is_object() {
                let mut r = get_list_of_children(i, tree, _recursion_level + 1);
                assert!(r.len() == 1);
                children_list.append(&mut r);
            } else {
                // Handle "KP" and "DHP"
                if i.is_string() {
                    let uid = i.as_str().unwrap();
                    assert!(uid.starts_with("kp") || uid.starts_with("dhp"));

                    let discourse: Option<u32>;
                    let section: Option<Section>;

                    if let Some(stripped) = uid.strip_prefix("kp") {
                        discourse = Some(stripped.parse::<u32>().unwrap());
                        section = None;
                    } else if let Some(stripped) = uid.strip_prefix("dhp") {
                        let parts = stripped.split('-').collect::<Vec<&str>>();
                        discourse = None;
                        section = Some(Section::SectionRange(
                            parts[0].parse::<u32>().unwrap(),
                            parts[1].parse::<u32>().unwrap(),
                        ));
                    } else {
                        panic!("unimplemented: {uid}");
                    }

                    let node = Node {
                        uid: uid.to_string(),
                        sutta: Sutta { discourse, section },
                        children: vec![],
                    };

                    tree.arena.push(node);
                    children_list.push(tree.arena.len() - 1);
                } else {
                    panic!("Unknown type: {i:?}");
                }
            }
        }
        return children_list;
    }

    if json_obj.is_object() {
        let mut children_list_root: Vec<usize> = Vec::new();
        for (key, value) in json_obj.as_object().unwrap() {
            assert!(value.is_array());
            let list = value.as_array().unwrap();
            if list[0].is_string() {
                let mut children_list: Vec<usize> = Vec::new();
                for i in list {
                    assert!(i.is_string());
                    let i = i.as_str().unwrap();
                    let blurb = extract_sutta_info(i);
                    let new_uid = generate_uid(&blurb, key);

                    let node = Node {
                        uid: new_uid,
                        sutta: blurb,
                        children: vec![],
                    };

                    tree.arena.push(node);
                    children_list.push(tree.arena.len() - 1);
                }

                let node = Node {
                    uid: key.clone(),
                    sutta: Sutta {
                        discourse: None,
                        section: None,
                    },
                    children: children_list,
                };

                assert!(!node.children.is_empty());
                tree.arena.push(node);
                children_list_root.push(tree.arena.len() - 1);
            } else {
                assert!(list[0].is_object());
                let cl = get_list_of_children(value, tree, _recursion_level + 1);

                let node = Node {
                    uid: key.clone(),
                    sutta: Sutta {
                        discourse: None,
                        section: None,
                    },
                    children: cl,
                };
                tree.arena.push(node);
                children_list_root.push(tree.arena.len() - 1);
            }
        }

        return children_list_root;
    }

    panic!("error")
}

// reads from the json files in "structure/tree/sutta/" and
// constructs the tree structure of the sutta
fn build_structure_tree(file_contents: &str) -> Tree {
    let mut tree = Tree { arena: vec![] };

    let json_obj: serde_json::Value = serde_json::from_str(file_contents).unwrap();

    assert!(json_obj.is_object());

    if let Some(i) = json_obj.as_object() {
        for (key, value) in i {
            let children = get_list_of_children(value, &mut tree, 1);

            let root_node = Node {
                uid: key.to_string(),
                sutta: Sutta {
                    discourse: Some(0),
                    section: Some(Section::Section(0)),
                },
                children,
            };

            tree.arena.push(root_node);
        }
    }

    tree
}

// Reads "misc/uid_expansion.json" into a HashMap
fn read_uid_expansion(base_dir: &str) -> HashMap<String, String> {
    let mut hashmap = HashMap::new();
    let full_path = base_dir.to_string() + "misc/uid_expansion.json";

    let contents_file = fs::read_to_string(full_path).expect("file read err");

    let json_obj: serde_json::Value =
        serde_json::from_str(&contents_file).expect("json parse error");

    for i in json_obj.as_array().unwrap() {
        if let Some(k) = i.as_object() {
            hashmap.insert(
                k["uid"].as_str().unwrap().to_string(),
                k["name"].as_str().unwrap().to_string(),
            );
        }
    }

    hashmap
}

const IO_ERROR: &str = "IO err";
const LINES: &str = "---";

// This will generate the markdown files
#[allow(clippy::too_many_lines, clippy::too_many_arguments)]
fn generate_page(
    tree: &Tree,
    tree_idx: usize,
    uid_expansion: &HashMap<String, String>,
    node_blurbs: &HashMap<String, (String, String)>,
    out_path: &String,
    root_uid: &str,
    out_subdir_path: &String,
    input_dir_path: &String,
    file_counter: &mut u32,
    #[allow(clippy::used_underscore_binding)] _recursion_level: u32,
) -> Result<String, String> {
    const EXTENSION_MARKDOWN: &str = ".markdown";
    let current_node = &tree.arena[tree_idx];

    let uid = &current_node.uid;
    let uid_name = if uid_expansion.contains_key(uid) {
        &uid_expansion[uid]
    } else {
        let tmp = if node_blurbs.contains_key(uid) {
            &node_blurbs[uid]
        } else {
            panic!("unknown uid");
        };
        &format!("{}({})", tmp.0, tmp.1)
    };

    if current_node.children.is_empty() {
        const SUTTA_DIR: &str = "sc_bilara_data/translation/en/sujato/sutta/";

        let permalink = format!("{out_subdir_path}/{uid}");
        let file_path = format!("{out_path}{out_subdir_path}/{uid}");

        let mut file = File::create(file_path.clone() + EXTENSION_MARKDOWN).unwrap();
        *file_counter += 1;

        writeln!(file, "{LINES}").expect(IO_ERROR);
        writeln!(file, "layout: default").expect(IO_ERROR);
        writeln!(file, "permalink: {permalink}.html").expect(IO_ERROR);
        writeln!(file, "title: \"{uid_name}\"").expect(IO_ERROR);
        writeln!(file, "{LINES}").expect(IO_ERROR);

        let file_path = if uid.starts_with("snp") || uid.starts_with("ud") {
            format!(
                "{}{}kn/{}/vagga{}/{}{}.{}_translation-en-sujato.json",
                input_dir_path,
                SUTTA_DIR,
                root_uid,
                current_node.sutta.discourse.unwrap(),
                root_uid,
                current_node.sutta.discourse.unwrap(),
                current_node.sutta.section.clone().unwrap()
            )
        } else if uid.starts_with("an") || uid.starts_with("sn") {
            let section = match current_node.sutta.section.clone().unwrap() {
                Section::Section(x) => format!("{x}"),
                Section::SectionRange(x, y) => format!("{x}-{y}"),
            };

            format!(
                "{}{}{}/{}{}/{}{}.{}_translation-en-sujato.json",
                input_dir_path,
                SUTTA_DIR,
                root_uid,
                root_uid,
                current_node.sutta.discourse.unwrap(),
                root_uid,
                current_node.sutta.discourse.unwrap(),
                section
            )
        } else if uid.starts_with("kp") || uid.starts_with("cp") || uid.starts_with("ja") {
            format!(
                "{}{}kn/{}/{}{}_translation-en-sujato.json",
                input_dir_path,
                SUTTA_DIR,
                root_uid,
                root_uid,
                current_node.sutta.discourse.unwrap()
            )
        } else if uid.starts_with("dhp") {
            let section = match current_node.sutta.section.clone().unwrap() {
                Section::Section(_) => panic!("Unsupported type"),
                Section::SectionRange(x, y) => format!("{x}-{y}"),
            };
            format!(
                "{input_dir_path}{SUTTA_DIR}kn/{root_uid}/{root_uid}{section}_translation-en-sujato.json"
            )
        } else if uid.starts_with("iti") {
            let current_discourse = current_node.sutta.discourse.unwrap();
            let vagga_idx = match current_discourse {
                1..11 => 1,
                11..21 => 2,
                21..28 => 3,
                28..38 => 4,
                38..50 => 5,
                50..60 => 6,
                60..70 => 7,
                70..80 => 8,
                80..90 => 9,
                90..100 => 10,
                100..113 => 11,
                _ => panic!("invalid idx"),
            };
            format!(
                "{input_dir_path}{SUTTA_DIR}kn/{root_uid}/vagga{vagga_idx}/{root_uid}{current_discourse}_translation-en-sujato.json"
            )
        } else if uid.starts_with("thag") || uid.starts_with("thig") {
            let section = match current_node.sutta.section.clone().unwrap() {
                Section::Section(x) => format!("{x}"),
                Section::SectionRange(_, _) => panic!("Unsupported type"),
            };
            format!(
                "{}{}kn/{}/{}{}.{}_translation-en-sujato.json",
                input_dir_path,
                SUTTA_DIR,
                root_uid,
                root_uid,
                current_node.sutta.discourse.unwrap(),
                section
            )
        } else {
            assert!(root_uid == "dn" || root_uid == "mn");
            format!(
                "{}{}{}/{}{}_translation-en-sujato.json",
                input_dir_path,
                SUTTA_DIR,
                root_uid,
                root_uid,
                current_node.sutta.discourse.unwrap()
            )
        };

        // Handle the case where there are no valid children. In such a
        // a case we should delete the file instead of showing an empty page
        if uid.starts_with("ja") && !fs::exists(&file_path).unwrap() {
            return Err(uid.clone());
        }

        write_contents(&file, &file_path);

        Ok(uid.clone())
    } else {
        let new_out_subdir_path = format!("{out_subdir_path}/{uid}");
        let dir_name = format!("{out_path}{new_out_subdir_path}");

        if let Err(e) = fs::create_dir(&dir_name) {
            assert!(
                (e.kind() == std::io::ErrorKind::AlreadyExists),
                "Unknown error: {e:?}"
            );
        }

        let file_path = format!("{out_path}{new_out_subdir_path}/{uid}{EXTENSION_MARKDOWN}");

        let mut file = File::create(&file_path).unwrap();
        let permalink = format!("{out_subdir_path}/{uid}");
        writeln!(file, "{LINES}").expect(IO_ERROR);
        writeln!(file, "layout: default").expect(IO_ERROR);
        writeln!(file, "permalink: {permalink}.html").expect(IO_ERROR);
        writeln!(file, "title: \"{uid_name}\"").expect(IO_ERROR);
        writeln!(file, "{LINES}").expect(IO_ERROR);

        let mut children_count = 0;
        for child in &current_node.children {
            let child_permalink = generate_page(
                tree,
                *child,
                uid_expansion,
                node_blurbs,
                out_path,
                root_uid,
                &new_out_subdir_path,
                input_dir_path,
                file_counter,
                _recursion_level + 1,
            );
            if child_permalink.is_err() {
                continue;
            }
            children_count += 1;

            let child_uid = &tree.arena[*child].uid;
            let (en, pli) = if node_blurbs.contains_key(child_uid) {
                &node_blurbs[child_uid]
            } else {
                println!("Absent: {child_uid}");
                &(child_uid.clone(), child_uid.clone())
            };

            let txt = if !en.is_empty() && !pli.is_empty() {
                &format!("{en}({pli})")
            } else if en.is_empty() && pli.is_empty() {
                child_uid
            } else if en.is_empty() {
                pli
            } else {
                en
            };

            let url = format!("{}/{}.html", uid, child_permalink.unwrap());

            writeln!(file, "1. [{txt}]({url})").expect(IO_ERROR);
        }

        if children_count == 0 {
            fs::remove_file(&file_path).expect(IO_ERROR);
            return Err(uid.clone());
        }
        *file_counter += 1;

        Ok(uid.clone())
    }
}

fn split_uid_to_parts(input: &str) -> Vec<u32> {
    let start_idx = get_idx_from_uid(input);
    let mut input = input[start_idx..].to_string();
    input = input.replace(':', ".");

    let map_closure = if input.contains('-') {
        |x: &str| -> u32 {
            x.split('-').collect::<Vec<&str>>()[0]
                .parse::<u32>()
                .unwrap()
        }
    } else {
        |x: &str| -> u32 { x.parse::<u32>().unwrap() }
    };

    input.split('.').map(map_closure).collect::<Vec<u32>>()
}

fn sort_func(x: &str, y: &str) -> std::cmp::Ordering {
    let parts_x = split_uid_to_parts(x);
    let parts_y = split_uid_to_parts(y);
    if x.starts_with("an") {
        assert!(parts_x.len() == 4);
        assert!(parts_y.len() == parts_x.len());
    }

    parts_x.cmp(&parts_y)
}

// Writes the sutta text to the file
fn write_contents(mut file: &std::fs::File, input_filename: &String) {
    let file_contents = fs::read_to_string(input_filename)
        .unwrap_or_else(|_| panic!("File not found: {input_filename}"));
    let json_obj: serde_json::Value = serde_json::from_str(&file_contents).expect("json parse err");

    if let Some(i) = json_obj.as_object() {
        let mut x = i.keys().collect::<Vec<&String>>().clone();
        // Even though the lines are in the correct order, while JSON parsing we lose the order
        // and therefore needs to be sorted
        x.sort_by(|x, y| sort_func(x, y));

        // if a line ends with ":" and the next line starts with "”"
        // we will render the following lines in italics(and prepend with spaces)
        // till we see a line that ends with "”". These variables are used
        // for this purpose.
        let mut should_add_spacing = false;
        let mut col_in_prev_line = false;
        let mut prev_section = 0;

        for key in x {
            let parts = split_uid_to_parts(key);
            let parts_len = parts.len();

            // Remove the <j> tag
            // https://discourse.suttacentral.net/t/what-is-the-j-tag/
            let line_to_write = i[key].as_str().unwrap().replace("<j>", "");
            let mut line_to_write = line_to_write.trim_end().to_string();

            // In some cases, we have a opening "”" but not a closing one,
            // we manually add a "”" to these lines as workaround.
            let str_list: [&str; 12] = [
                "the specific deeds performed in the past to obtain each mark.",
                "Once they’ve gone forth, they live restrained in the monastic code, conducting themselves well and resorting for alms in suitable places. Seeing danger in the slightest fault, they keep the rules they’ve undertaken. They act skillfully by body and speech. They’re purified in livelihood and accomplished in ethical conduct. They guard the sense doors, have mindfulness and situational awareness, and are content.",
                "Please instruct me. It will be for my lasting welfare and happiness.’",
                "That’s why I have taught and pointed out such teachings as not categorical.",
                "and their capital city was named Bandhumatī.",
                "The Benefits of Ethical Conduct",
                "These are the five drawbacks for an unethical person because of their failure in ethics.",
                "They meditate observing an aspect of principles—keen, aware, and mindful, rid of covetousness and displeasure for the world.",
                "It is due to undertaking skillful qualities that merit grows.",
                "When their body breaks up, after death, they’re reborn in a good place, a heavenly realm.",
                "the release from all ties.",
                "Some defilements should be given up by seeing, some by restraint, some by using, some by enduring, some by avoiding, some by dispelling, and some by developing.",
            ];

            if str_list.contains(&line_to_write.as_str()) {
                line_to_write += "”";
            }

            // Detect headings from the lines and render them appropriately
            let mut prefix = if line_to_write.is_empty() {
                ""
            } else if parts[parts_len - 2] == 0 {
                "## "
            } else if parts[parts_len - 1] == 0 {
                "### "
            } else {
                ""
            };

            if col_in_prev_line && line_to_write.starts_with("“") {
                should_add_spacing = true;
            }

            let suffix = if should_add_spacing {
                if !prefix.is_empty() {
                    println!("{line_to_write}");
                }

                prefix = "&emsp;*";
                "*"
            } else {
                ""
            };

            let current_section = parts[parts_len - 2];
            if current_section > prev_section && !should_add_spacing {
                writeln!(file).expect(IO_ERROR);
                prev_section = current_section;
            }
            // Add two newlines at end to add a linebreak in the output
            writeln!(file, "{prefix}{line_to_write}{suffix}  ").expect(IO_ERROR);

            if line_to_write.ends_with(':') {
                col_in_prev_line = true;
            } else if line_to_write.ends_with("”")
                || line_to_write.ends_with("”’")
                || line_to_write.contains("”")
            {
                should_add_spacing = false;
                col_in_prev_line = false;
            }
        }
    }
}
fn generate_markdown(
    tree: &Tree,
    uid_expansion: &HashMap<String, String>,
    node_blurbs: &HashMap<String, (String, String)>,
    in_path: &String,
    out_path: &String,
    root_uid: &str,
) -> Result<String, String> {
    fs::create_dir_all(out_path.to_string() + "sutta").expect("io fail");
    let mut counter: u32 = 0;
    let result = generate_page(
        tree,
        tree.arena.len() - 1,
        uid_expansion,
        node_blurbs,
        out_path,
        root_uid,
        &"sutta".to_string(),
        in_path,
        &mut counter,
        0,
    );
    println!("{root_uid} {counter}");
    result
}

fn get_node_blurbs(
    en_file_contents: &str,
    pli_file_contents: &str,
) -> HashMap<String, (String, String)> {
    let en_blurbs: serde_json::Value = serde_json::from_str(en_file_contents).unwrap();
    let pli_blurbs: serde_json::Value = serde_json::from_str(pli_file_contents).unwrap();

    let mut node_blurbs: HashMap<String, (String, String)> = HashMap::new();

    if let Some(x) = en_blurbs.as_object() {
        for (key, value) in x {
            let k = key[key.find('.').unwrap() + 1..].to_string();
            let v = (
                value.as_str().unwrap().to_string(),
                pli_blurbs[key].as_str().unwrap().to_string(),
            );
            node_blurbs.insert(k, v);
        }
    }

    if let Some(x) = pli_blurbs.as_object() {
        for (key, _) in x {
            let k = key[key.find('.').unwrap() + 1..].to_string();
            if node_blurbs.contains_key(&k) {
                continue;
            }

            let v = (String::new(), pli_blurbs[key].as_str().unwrap().to_string());
            node_blurbs.insert(k, v);
        }
    }

    node_blurbs
}

fn print_usage_and_exit(args: &[String]) {
    println!("Usage: {} SC_DATA_DIR DEST_DIR", args[0]);
    std::process::exit(1);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        print_usage_and_exit(&args);
    }
    let dir_path = args[1].clone();
    let output_path = args[2].clone();

    let sutta_list: [&str; 13] = [
        "dn", "mn", "sn", "an", "kp", "dhp", "ud", "iti", "ja", "cp", "thig", "thag", "snp",
    ];

    let timer = std::time::Instant::now();

    for i in sutta_list {
        let tree_path = format!("structure/tree/sutta/{i}-tree.json");
        let en_translations = format!(
            "sc_bilara_data/translation/en/sujato/name/sutta/{i}-name_translation-en-sujato.json"
        );
        let pli_text =
            format!("sc_bilara_data/root/misc/site/name/sutta/{i}-name_root-misc-site.json");

        let contents_tree =
            fs::read_to_string(dir_path.clone() + &tree_path).expect(IO_ERROR);

        let contents_en_translations =
            fs::read_to_string(dir_path.clone() + &en_translations).expect(IO_ERROR);

        let contents_pli_text =
            fs::read_to_string(dir_path.clone() + &pli_text).expect(IO_ERROR);

        let node_blurbs = get_node_blurbs(&contents_en_translations, &contents_pli_text);
        let tree = build_structure_tree(&contents_tree);
        let uid_expansion = read_uid_expansion(&dir_path);

        generate_markdown(
            &tree,
            &uid_expansion,
            &node_blurbs,
            &dir_path,
            &output_path,
            i,
        )
        .expect("Markdown generation failed");
    }
    println!("Completed in: {:.3?}", timer.elapsed());
}
