use std::fs;

fn main() {
    println!("Hello, world!");

    let contents : String =
        fs::read_to_string("input-day05").expect("Something went wrong reading the file");
    // let contents = "dabAcCaCBAcCcaDA".to_string();

    let all_letters = "abcdefghijklmnopqrstuvwxyz";
    for c in all_letters.chars() {
        let contents_without_char = remove_char(&contents, c);
        // println!("char {}, len: {}, contents: {}", c, contents_without_char.len(), contents_without_char);
        let len_after_react = react(contents_without_char);
        println!("char {}, len after react: {}", c, len_after_react);
    }
}

fn remove_char(contents: &String, to_remove_char: char) -> String {
    let mut vec = Vec::new();
    for c in contents.chars() {
        if c.to_ascii_lowercase() == to_remove_char {
        } else {
            vec.push(c);
        }
    }

    return vec.into_iter().collect();
}

fn react(contents: String) -> usize {
    let mut vec = Vec::new();

    // println!("{}", contents);
    let mut _i = 0;
    for c in contents.chars() {
        if c.is_ascii_alphabetic() {
            // if i > 30 { break; }
            _i += 1;

            // print!("{}, current stack: {:?}, next character: {}", i, vec, c);
            let top = vec.pop();
            match top {
                None => {
                    // println!(", nothing on stack, decided to push: {}", c);
                    vec.push(c);
                }
                Some(t) => {
                    if is_collapse(t, c) {
                        // println!(", last on stack is collapsable, throwing away {} from stack and new char {}", t, c);

                    } else {
                        // println!(", last on stack is not collapsable, pushing: {}", c);
                        vec.push(t);
                        vec.push(c);
                    }
                }

            }
        }
    }

    // println!("done.  vec: {:?}", vec);
    // println!("done.  vec len: {}", vec.len());
    return vec.len();
}

fn is_collapse(a: char, b: char) -> bool {
    ((a.to_ascii_uppercase() == b) || (a.to_ascii_lowercase() == b)) && (a != b)
}
