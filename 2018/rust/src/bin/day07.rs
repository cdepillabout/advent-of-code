
use regex::Regex;
use std::collections::{HashMap, HashSet};


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct Above(char, char);

fn main() {
    let input = include_str!("../../input-day07-simple");

    let re = Regex::new(r"Step (?P<before>.) must be finished before step (?P<after>.) can begin.").unwrap();

    let dirs: Vec<Above> = re.captures_iter(input).map(|caps| {
        let before = caps.name("before").unwrap().as_str().chars().next().unwrap();
        let after = caps.name("after").unwrap().as_str().chars().next().unwrap();
        Above(before, after)
    }).collect();

    let mut hashmap: HashMap<char, HashSet<char>> = HashMap::new();
    let mut hashmap_reverse: HashMap<char, HashSet<char>> = HashMap::new();

    for Above(before, after) in dirs {

        hashmap.entry(before).and_modify(|afters: &mut HashSet<char>| {
            afters.insert(after);
        }).or_insert({
            let mut x = HashSet::new();
            x.insert(after);
            x
        });

        hashmap_reverse.entry(after).and_modify(|befores: &mut HashSet<char>| {
            befores.insert(before);
        }).or_insert({
            let mut x = HashSet::new();
            x.insert(before);
            x
        });
    }

    // add empty things
    {
        let hashmap_keys: HashSet<char> = hashmap.keys().copied().collect();
        let hashmap_reverse_keys: HashSet<char> = hashmap_reverse.keys().copied().collect();

        // Characters that only exist in the reverse map.
        let only_in_reverse_map: Vec<char> = hashmap_reverse_keys.difference(&hashmap_keys).cloned().collect();

        for c in only_in_reverse_map {
            hashmap.insert(c, HashSet::new());
        }
    }

    // println!("hashmap {:?}", hashmap);
    dbg!(&hashmap);
    dbg!(&hashmap_reverse);

    let mut i = 0;


    let mut order: Vec<char> = vec![];

    println!("_______________________________________________________________________________________________________________________________");

    loop {
        dbg!(i);

        let hashmap_keys: HashSet<char> = hashmap.keys().copied().collect();
        let hashmap_reverse_keys: HashSet<char> = hashmap_reverse.keys().copied().collect();

        // Characters that only exist in the before column.
        let mut first_chars: Vec<char> = hashmap_keys.difference(&hashmap_reverse_keys).cloned().collect();
        first_chars.sort();

        dbg!(&first_chars);

        // The direct dependencies from the characters that only exist in the before column.
        let direct_deps: HashSet<char> = first_chars.iter().flat_map(|c| hashmap.get(c).unwrap() ).copied().collect::<HashSet<char>>();

        dbg!(&direct_deps);

        for c in direct_deps {

            // The reverse dependencies for a char.
            let stuff: &mut HashSet<char> = hashmap_reverse.get_mut(&c).unwrap();

            // Remove all of the before deps from the list of reverse deps.
            for d in &first_chars {
                stuff.remove(d);
            }

            if stuff.is_empty() {
                hashmap_reverse.remove(&c);
            }
        }

        for c in &first_chars {
            order.push(*c);
            hashmap.remove(c);
        }

        dbg!(&order);
        dbg!(&hashmap);
        dbg!(&hashmap_reverse);

        if hashmap.is_empty() {
            break;
        }

        i+=1;
        println!("_______________________________________________________________________________________________________________________________");

        // let mut line = String::new();
        // let _ = std::io::stdin().read_line(&mut line).expect("Failed to read line");
    }

    let mut leftovers: Vec<char> = hashmap_reverse.keys().copied().collect();
    leftovers.sort();

    for c in leftovers {
        order.push(c);
    }

    dbg!(&order);

    println!("{}", order.iter().collect::<String>());

}
