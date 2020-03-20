use core::iter::repeat_with;

pub fn is_prime(num: &u32) -> bool {
    if *num == 2 {
        true
    } else {
        let upperbound = ((*num as f64).sqrt().ceil() as u32);
        (2..=upperbound).all(|f| num % f != 0)
    }
}

pub fn nth(n: u32) -> u32 {
    let mut num = 2;
    repeat_with(|| -> u32 {
        num = num + 1;
        num - 1
    }).filter(|c| is_prime(c)).nth(n as usize).expect("No prime.")
}
