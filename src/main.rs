// Invoke as `cargo run` from top-level repo

use core::marker::Copy;
use std::cmp::{max, Ord};
use std::collections::HashMap;
use std::convert::From;
use std::fs;
use std::ops::{Add, Div, Sub};
use std::time::SystemTime;

fn mass_to_fuel_i64(mass: i64) -> i64 {
    max(0, mass / 3 - 2)
}

// "you might be able to shorten it by using the num (num-traits) crate" (stephaneyfx on Discord #beginners)
fn mass_to_fuel<T>(mass: T) -> T
where
    T: Div<Output = T> + Sub<Output = T> + From<i8> + Ord,
{
    max(T::from(0), mass / T::from(3) - T::from(2))
}

fn mass_to_total_fuel<T>(mass: T) -> T
where
    T: Copy + Add<Output = T> + Div<Output = T> + Sub<Output = T> + From<i8> + Ord,
{
    let mut fuel = mass_to_fuel(mass);
    let mut ret = fuel;
    while fuel > T::from(0) {
        fuel = mass_to_fuel(fuel);
        ret = fuel + ret;
    }
    ret
}

fn intcode(v: &mut Vec<i32>) {
    let mut pc: usize = 0;
    while v[pc] != 99 {
        match v[pc] {
            1 | 2 => {
                let a = v[v[pc + 1] as usize];
                let b = v[v[pc + 2] as usize];
                let d = v[pc + 3 as usize] as usize;
                v[d] = if v[pc] == 1 { a + b } else { a * b };
                pc += 4;
            }
            _ => {}
        }
    }
}

type PathMap = HashMap<(i32, i32), i32>;
fn string_to_pathmap(path1: &str) -> PathMap {
    let mut dict: PathMap = HashMap::new();
    let mut x = 0i32;
    let mut y = 0i32;
    let mut len = 0;
    for s in path1.split(',') {
        let step: i32 = s[1..].parse().expect("parse err");
        match s.as_bytes()[0] {
            b'L' => {
                for i in 1..step {
                    dict.insert((x - i, y), len + i);
                }
                x -= step;
            }
            b'R' => {
                for i in 1..step {
                    dict.insert((x + i, y), len + i);
                }
                x += step;
            }
            b'U' => {
                for i in 1..step {
                    dict.insert((x, y + i), len + i);
                }
                y += step;
            }
            b'D' => {
                for i in 1..step {
                    dict.insert((x, y - i), len + i);
                }
                y -= step;
            }
            _ => {}
        };
        len += step;
    }
    dict
}
// I initially wrote this to take an actual PathMap instead of a reference,
// but that'd move the map here, and it'd be unavaialble to the caller afterwards.
fn best_intersection(m1: &PathMap, m2: &PathMap, manhattan: bool) -> Option<i32> {
    m1.keys()
        .filter_map(|xy| {
            if m2.contains_key(xy) {
                Some(if manhattan {
                    (xy.0).abs() + xy.1.abs()
                } else {
                    m1.get(xy).unwrap_or(&0) + m2.get(xy).unwrap_or(&0)
                })
            } else {
                None
            }
        })
        .min()
}

fn ok_password_a(n: i32) -> bool {
    let s = n.to_string();
    let bytes = s.as_bytes();
    let mut monotonic = true;
    let mut two_adjacent = false;
    for i in 1usize..bytes.len() {
        monotonic = bytes[i - 1] <= bytes[i];
        two_adjacent |= bytes[i - 1] == bytes[i];
        if !monotonic {
            return false;
        }
    }
    monotonic && two_adjacent
}
fn ok_password_arithmetic_a(n: i32) -> bool {
    let d6 = n % 10;
    let d5 = (n / 10) % 10;
    let d4 = (n / 100) % 10;
    let d3 = (n / 1_000) % 10;
    let d2 = (n / 10_000) % 10;
    let d1 = (n / 100_000) % 10;
    // d1 d2 d3 d4 d5 d6
    return (d1 == d2 || d2 == d3 || d3 == d4 || d4 == d5 || d5 == d6)
        && (d1 <= d2 && d2 <= d3 && d3 <= d4 && d4 <= d5 && d5 <= d6);
}
fn ok_password_window_a(n: i32) -> bool {
    let s = n.to_string();
    let bytes = s.as_bytes();
    bytes.windows(2).all(|c| c[0] <= c[1]) && bytes.windows(2).any(|c| c[0] == c[1])
}
fn ok_password_arithmetic_b(n: i32) -> bool {
    let d6 = n % 10;
    let d5 = (n / 10) % 10;
    let d4 = (n / 100) % 10;
    let d3 = (n / 1_000) % 10;
    let d2 = (n / 10_000) % 10;
    let d1 = (n / 100_000) % 10;
    // d1 d2 d3 d4 d5 d6
    return ((d1 == d2 && d2 != d3)
        || (d1 != d2 && d2 == d3 && d3 != d4)
        || (d2 != d3 && d3 == d4 && d4 != d5)
        || (d3 != d4 && d4 == d5 && d5 != d6)
        || (d4 != d5 && d5 == d6))
        && (d1 <= d2 && d2 <= d3 && d3 <= d4 && d4 <= d5 && d5 <= d6);
}

fn main() {
    {
        assert_eq!(2, mass_to_fuel_i64(12));
        assert_eq!(2, mass_to_fuel(12));
        assert_eq!(2, mass_to_fuel(14));
        assert_eq!(654, mass_to_fuel(1969));
        assert_eq!(33583, mass_to_fuel(100756));

        let input1: Vec<i32> = fs::read_to_string(String::from("input.1.txt"))
            .expect("Something went wrong reading the file")
            .split_ascii_whitespace()
            .map(|s| s.parse().expect("failed to parse"))
            .collect();
        let answer1a = input1
            .iter()
            .copied()
            .map(|x| mass_to_fuel(x))
            .fold(0, |acc, x: i32| acc + x);
        assert_eq!(3386686, answer1a);
        //
        assert_eq!(2, mass_to_total_fuel(12));
        assert_eq!(2, mass_to_total_fuel(14));
        assert_eq!(966, mass_to_total_fuel(1969));
        assert_eq!(50346, mass_to_total_fuel(100756));

        let answer1b = input1
            .into_iter() // moves the Vec!
            .map(|x| mass_to_total_fuel(x))
            .fold(0, |acc, x: i32| acc + x);
        assert_eq!(5077155, answer1b);
    }
    {
        {
            let mut short_program: Vec<i32> = vec![1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50];
            intcode(&mut short_program);
            assert_eq!(
                &short_program,
                &[3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
            );
        }
        {
            let mut short_program: Vec<i32> = vec![1, 1, 1, 4, 99, 5, 6, 0, 99];
            intcode(&mut short_program);
            assert_eq!(&short_program, &[30, 1, 1, 4, 2, 5, 6, 0, 99]);
        }
        if let Some(s) = fs::read_to_string(String::from("input.2.txt"))
            .expect("read error")
            .lines()
            .next()
        {
            let program: Vec<i32> = s.split(',').map(|s| s.parse().expect("parseerr")).collect();
            {
                let mut copy = program.clone();
                copy[1] = 12;
                copy[2] = 2;
                intcode(&mut copy);
                assert_eq!(4576384, copy[0]);
            }
            let (noun, verb) = {
                let mut tuple = (-1i32, -2i32);
                'outer: for noun in 0i32..99 {
                    for verb in 0i32..99 {
                        let mut copy = program.clone();
                        copy[1] = noun;
                        copy[2] = verb;
                        intcode(&mut copy);
                        if copy[0] == 19690720 {
                            tuple.0 = noun;
                            tuple.1 = verb;
                            break 'outer;
                        }
                    }
                }
                tuple
            };
            assert_eq!(5398, 100 * noun + verb);
        }
    }
    {
        {
            let path1 = String::from("R8,U5,L5,D3");
            let m1 = string_to_pathmap(&path1);
            let m2 = string_to_pathmap("U7,R6,D4,L4");
            assert_eq!(Some(6), best_intersection(&m1, &m2, true));
            assert_eq!(Some(30), best_intersection(&m1, &m2, false));
        }
        {
            let m1 = string_to_pathmap("R75,D30,R83,U83,L12,D49,R71,U7,L72");
            let m2 = string_to_pathmap("U62,R66,U55,R34,D71,R55,D58,R83");
            assert_eq!(Some(159), best_intersection(&m1, &m2, true));
            assert_eq!(Some(610), best_intersection(&m1, &m2, false));
        }
        {
            let m1 = string_to_pathmap("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51");
            let m2 = string_to_pathmap("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7");
            assert_eq!(Some(135), best_intersection(&m1, &m2, true));
            assert_eq!(Some(410), best_intersection(&m1, &m2, false));
        }
        {
            let input3: Vec<PathMap> = fs::read_to_string(String::from("input.3.txt"))
                .expect("Something went wrong reading the file")
                .split_ascii_whitespace()
                .take(2)
                .map(string_to_pathmap)
                .collect();
            assert_eq!(Some(375), best_intersection(&input3[0], &input3[1], true));
            assert_eq!(
                Some(14746),
                best_intersection(&input3[0], &input3[1], false)
            );
            {
                // alternative:
                let fid = fs::read_to_string(String::from("input.3.txt"))
                    .expect("Something went wrong reading the file");
                let mut lines3 = fid.lines();
                let p1 = string_to_pathmap(lines3.next().expect("no first line"));
                let p2 = string_to_pathmap(lines3.next().expect("no second line"));
                assert_eq!(Some(375), best_intersection(&p1, &p2, true));
            }
        }
    }
    {
        assert_eq!(true, ok_password_a(111111));
        assert_eq!(true, ok_password_window_a(111111));
        assert_eq!(false, ok_password_a(223450));
        assert_eq!(false, ok_password_window_a(223450));
        assert_eq!(false, ok_password_a(123789));
        assert_eq!(false, ok_password_window_a(123789));

        {
            let r = 183564i32..657474;
            let now = SystemTime::now();
            let res = r.fold(0, |acc, n: i32| acc + i32::from(ok_password_window_a(n)));
            let elapsed = now.elapsed();
            assert_eq!(1610, res);
            println!("{:?} s elapsed", elapsed.unwrap().as_secs_f64());
        }
        {
            let r = 183564i32..657474;
            let now = SystemTime::now();
            let res = r.fold(0, |acc, n: i32| {
                acc + i32::from(ok_password_arithmetic_a(n))
            });
            let elapsed = now.elapsed();
            assert_eq!(1610, res);
            println!("{:?} s elapsed", elapsed.unwrap().as_secs_f64());
        }
        {
            let r = 183564i32..657474;
            let now = SystemTime::now();
            let res = r.fold(0, |acc, n: i32| {
                acc + i32::from(ok_password_arithmetic_b(n))
            });
            let elapsed = now.elapsed();
            assert_eq!(1104, res);
            println!("{:?} s elapsed", elapsed.unwrap().as_secs_f64());
        }
    }
    println!("Yay!");
}
