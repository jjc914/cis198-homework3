pub trait Summary {
	fn summarize(&self, lines: usize) -> String;
}