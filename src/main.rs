// Invoke as `cargo run` from top-level repo

use core::marker::Copy;
use std::cmp::max;
use std::cmp::Ord;
use std::convert::From;
use std::fs;
use std::ops::Add;
use std::ops::Div;
use std::ops::Sub;

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

fn main() {
    assert_eq!(2, mass_to_fuel_i64(12));
    assert_eq!(2, mass_to_fuel(12));
    assert_eq!(2, mass_to_fuel(14));
    assert_eq!(654, mass_to_fuel(1969));
    assert_eq!(33583, mass_to_fuel(100756));

    let input1: Vec<i32> = fs::read_to_string(String::from("input.1.txt"))
        .expect("Something went wrong reading the file")
        .split_ascii_whitespace()
        .map(|s| s.parse().expect("Please type a number!"))
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

    println!("Yay!");
}
