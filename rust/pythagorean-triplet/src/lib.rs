use std::collections::HashSet;

pub fn find(sum: u32) -> HashSet<[u32; 3]> {
    let mut result = HashSet::new();
    for x in 1..sum {
        for y in x..sum {
            let z = ((x * x + y * y) as f64).sqrt();
            if z.floor() == z && x + y + (z as u32) == sum {
                result.insert([x, y, (z as u32)]);
            }
        }
    }
    return result;
}
