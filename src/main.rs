// Invoke as `cargo run` from top-level repo

use core::marker::Copy;
use std::cmp::{max, Ord};
use std::convert::From;
use std::fs;
use std::ops::{Add, Div, Sub};

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

fn main() {
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

    assert_eq!(2, mass_to_total_fuel(12));
    assert_eq!(2, mass_to_total_fuel(14));
    assert_eq!(966, mass_to_total_fuel(1969));
    assert_eq!(50346, mass_to_total_fuel(100756));

    let answer1b = input1
        .into_iter() // moves the Vec!
        .map(|x| mass_to_total_fuel(x))
        .fold(0, |acc, x: i32| acc + x);
    assert_eq!(5077155, answer1b);

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

    println!("Yay!");
}
