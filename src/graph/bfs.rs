use std::hash::Hash;
use std::collections::HashSet;
use std::collections::VecDeque;

use crate::graph::*;

pub struct BFS<T, E, F>
where
    T: Eq + PartialEq + Hash,
    E: Eq + PartialEq,
    F: Fn(&T, &HashMap<T, HashMap<T, E>>) -> Vec<(T /* dest vid */, E /* edge label */)>,
{
    next_adjacent: F,
    adjacency_map: HashMap<T /* origin vid */, HashMap<T /* dest vid */, E /* edge label */>>,

    visited: HashSet<T /* only need to know info of the visited nodes, dont care about the edges */>,
    to_visit: VecDeque<(T /* dest vid */, Option<E> /* edge label */)>,
}

impl<T, E, F> BFS<T, E, F> 
where
    T: Eq + PartialEq + Hash,
    E: Eq + PartialEq,
    F: Fn(&T, &HashMap<T, HashMap<T, E>>) -> Vec<(T /* dest vid */, E /* edge label */)>,
{
    pub fn new(start: T, next_adjacent: F, adjacency_map: HashMap<T, HashMap<T, E>>) -> Self {
        Self {
            next_adjacent: next_adjacent, // this function will return the vector of all the next vertices to use in
                        // the search
            adjacency_map: adjacency_map,

            visited: HashSet::new(),
            to_visit: VecDeque::from([(start, None)]), // stack
        }
    }
}

// /*
//  * dfs and bfs use stack/queue. 
//  */

impl<T, E, F> Iterator for BFS<T, E, F>
where
    T: Eq + PartialEq + Copy + Hash,
    E: Eq + PartialEq,
    F: Fn(&T, &HashMap<T, HashMap<T, E>>) -> Vec<(T /* dest vid */, E /* edge label */)>,
{
    type Item = (T, Option<E>);

    fn next(&mut self) -> Option<Self::Item> {
        let visiting = match self.to_visit.pop_front() {
            Some(v) => v,
            None => {
                return None;
            }
        };
        if !self.visited.contains(&(visiting.0)) {
            for (adjacent, edge) in (self.next_adjacent)(&(visiting.0), &self.adjacency_map) { // get the adjacent 
                if !self.visited.contains(&adjacent) {
                    self.to_visit.push_back((adjacent, Some(edge)));
                }
            }
            self.visited.insert(visiting.0);           
        }
        Some(visiting)
    }
}