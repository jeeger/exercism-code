use itertools::unfold;

fn to_digits(num: &mut u32) -> Option<u32> {
    if *num > 0 {
        let result = *num % 10;
        *num = *num / 10;
        Some(result)
    } else {
        None
    }
}

pub fn is_armstrong_number(num: u32) -> bool {
    let mut rem = num;
    let digits: Vec<u32> = unfold((), move |_| -> Option<u32> {
        if rem > 0 {
            let result = rem % 10;
            rem = rem / 10;
            Some(result)
        } else {
            None
        }
    }).collect();
    let size = digits.len();
    digits.iter().fold(0, |sum, num| sum + num.pow(size as u32)) == num
}
