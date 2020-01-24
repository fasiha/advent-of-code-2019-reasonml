use std::cmp::max;
use std::cmp::Ord;
use std::convert::From;
use std::ops::Div;
use std::ops::Sub;

fn mass_to_fuel_i64(mass: i64) -> i64 {
    max(0, mass / 3 - 2)
}

fn mass_to_fuel<T: Div<Output = T> + Sub<Output = T> + From<i8> + Ord>(mass: T) -> T {
    max(T::from(0), mass / T::from(3) - T::from(2))
}

fn main() {
    assert_eq!(2, mass_to_fuel_i64(12));
    assert_eq!(2, mass_to_fuel(12));
    assert_eq!(2, mass_to_fuel(14));
    assert_eq!(654, mass_to_fuel(1969));
    assert_eq!(33583, mass_to_fuel(100756));
    println!("Yay!");
}
