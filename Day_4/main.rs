
const SIZE: usize = 21;

// fn to_num(t: &[usize]) -> usize {
//     t.iter().fold(0, |b, &x| b * 10 + x)
// }

fn max_count(t: &[usize]) -> bool {
    let mut count: usize = 1;
    let mut last: usize = t[0];

    for &x in t.get(1..).unwrap() {
        if x == last {
            count += 1;
        } else {
            if count == 2 {
                return true;
            } else {
                count = 1;
                last = x;
            }
        }
    }

    count == 2
}

fn is_good(t: &[usize]) -> bool {
    max_count(t)
}

fn count(num: &mut [usize], idx: usize, last: usize) -> usize {
    if idx == SIZE {
        // println!("{:?}", num);
        return if is_good(num) { 1 } else { 0 };
    }

    (last..10).map(|i| { num[idx] = i; count(num, idx + 1, i)}).sum()
}

fn main() {
    println!("{}", count(&mut [0;SIZE], 0, 0));
}
