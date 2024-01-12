pub trait Reachable {
    type T;

    fn can_reach(&self, start: &Self::T, end: &Self::T) -> bool;
    fn distance(&self, start: &Self::T, end: &Self::T) -> Option<usize>;
}