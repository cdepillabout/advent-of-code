use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct Point {
  x: i32,
  y: i32,
}

// . . . . . . . . . . .
// . . . . . . . . . . .
// . . . A . . . . . C .
// . . . . . . . . . . .
// . . . . B . . . . . .
// . . . . . . . . . . .
// . . . . . . . . . . .
// . . . . . . . . . . .
// . . . . . . . . . E .
// . . . D . . . . . . .
// . . . . . . . . . . .
// . . . . . . . . . . .


enum PointClosest {
    NoClosest,
    OneClosest { index: usize, dist: i32 },
    MultiClosest { dist: i32 },
}


fn main() {
    println!("Hello, world!");

    let input = include_str!("../../input-day06");

    let lines = input.lines();

    let split_lines = lines.map(|line| line.split(", "));

    let mut all_points = Vec::new();

    for mut line in split_lines {
        let x_str = line.next().expect("Should have gotten an X point");
        let x = x_str.parse::<i32>().expect("X should have been int");
        let y_str = line.next().expect("Should have gotten an Y point");
        let y = y_str.parse::<i32>().expect("Y should have been int");

        all_points.push(Point {x,y})
    };

    let xs = all_points.iter().map(|point| point.x);
    let ys = all_points.iter().map(|point| point.y);

    let x_min: i32 = xs.clone().min().expect("vec should not be empty");
    let x_max: i32 = xs.clone().max().expect("vec should not be empty");
    let y_min: i32 = ys.clone().min().expect("vec should not be empty");
    let y_max: i32 = ys.clone().max().expect("vec should not be empty");

    let mut closest_points: HashMap<Point, usize> = HashMap::new();
    let mut infinite_points: HashSet<usize> = HashSet::new();

    // Loop over all the (x,y) points in the bounding box for all input coordinates.
    for x in x_min .. (x_max + 1) {
        for y in y_min .. (y_max + 1) {

            // Figure out the point closest to this (x,y) point.
            let mut point_closest = PointClosest::NoClosest;

            // A point representing an (x,y) point.
            let my_point = Point {x, y};

            for (index, point) in all_points.iter().enumerate() {
                let dist = manhattan_dist(*point, my_point);

                match point_closest {
                    PointClosest::NoClosest =>
                        point_closest = PointClosest::OneClosest{index, dist},
                    PointClosest::OneClosest{index: _, dist: old_dist} =>
                        if old_dist == dist {
                            point_closest = PointClosest::MultiClosest{dist}
                        } else if dist < old_dist {
                            point_closest = PointClosest::OneClosest{index, dist}
                        },
                    PointClosest::MultiClosest{dist: old_dist} =>
                        if dist < old_dist {
                            point_closest = PointClosest::OneClosest{index, dist}
                        },
                }
            };

            // If there is only one input coordinate closest to this point, then insert it in
            // closest_points.
            if let PointClosest::OneClosest { index, dist: _ } = point_closest {
                closest_points.insert(my_point, index);
            };

            // If this is on one of the edges of the bounding box around all input coordinates, add
            // it to our list of infinite points.
            if x == x_min || x == x_max || y == y_min || y == y_max {
                if let PointClosest::OneClosest { index, dist: _ } = point_closest {
                    infinite_points.insert(index);
                };
            };
        };
    };

    // The number of closest points for the index of each of the input coordinates.
    let num_vals: HashMap<usize, u32> = collect_num_vals(closest_points);
    let vec_num_vals: Vec<(usize, u32)> = num_vals.iter().map(|(usize, u32)| (*usize, *u32)).collect();

    // The same thing above, sorted by the number of closest points.
    let mut sorted_vec: Vec<(usize, u32)> = vec_num_vals.clone();
    sorted_vec.sort_by(|(_, num1), (_, num2)| num1.cmp(num2));

    // The same thing above, but with a boolean indicating whether each input coordinate is an
    // infinite coordinate.
    let sorted_vec_with_inf: Vec<(usize, u32, bool)> =
        sorted_vec
            .iter()
            .map(|(index, num)|
                    (*index, *num, infinite_points.contains(index)))
            .collect();

    println!("num_vals: {:?}\n", num_vals);
    println!("vec_num_vals: {:?}\n", vec_num_vals);
    println!("sorted_vec: {:?}\n", sorted_vec);
    println!("sorted_vec_with_inf: {:?}\n", sorted_vec_with_inf);
}

fn manhattan_dist(p1: Point, p2: Point) -> i32 {
    (p1.x - p2.x).abs() + (p1.y - p2.y).abs()
}

// Collect the number of each distinct value in a HashMap.
fn collect_num_vals<Key, Value>(hashmap: HashMap<Key, Value>) -> HashMap<Value, u32>
where
    Value: Copy + Eq + std::hash::Hash
{
    let mut new_hashmap: HashMap<Value, u32> = HashMap::new();

    for val in hashmap.values() {
        let does_already_exist: Option<u32> = new_hashmap.get(val).copied();

        match does_already_exist {
            Some(existing_num) => new_hashmap.insert(*val, existing_num + 1),
            None => new_hashmap.insert(*val, 1),
        };
    }

    new_hashmap
}
