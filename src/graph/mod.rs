pub mod bfs;
pub mod dfs;
pub mod reachable;
pub mod summary;

use std::collections::HashMap;
use std::hash::Hash;
use std::fmt;
use std::ops::{Index, IndexMut};
use std::rc::Rc;
use std::ops::Deref;
use std::fs;
use std::fs::File;
use std::io::prelude::*;

use serde::{Serialize, Deserialize};
use serde::de::DeserializeOwned;

use crate::graph::dfs::DFS;
use crate::graph::bfs::BFS;
use crate::graph::reachable::Reachable;
use crate::graph::summary::Summary;

/* 
 * errors
 */
#[derive(Debug, Eq, PartialEq)]
pub struct VertexExistsError;

#[derive(Debug, Eq, PartialEq)]
pub struct VertexMissingError;

#[derive(Debug, Eq, PartialEq)]
pub struct OriginMissingError;

#[derive(Debug, Eq, PartialEq)]
pub struct DestinationMissingError;

#[derive(Debug, Eq, PartialEq)]
pub struct EdgeExistsError;

#[derive(Debug, Eq, PartialEq)]
pub enum EdgePushError {
    OriginMissingError,
    DestinationMissingError,
    EdgeExistsError,
}

#[derive(Debug, Eq, PartialEq)]
pub enum EdgeRemoveError {
    OriginMissingError,
    DestinationMissingError,
    EdgeMissingError,
}

#[derive(Debug, Eq, PartialEq)]
pub enum EdgeGetError {
    OriginMissingError,
    DestinationMissingError,
    EdgeMissingError,
}

#[derive(Debug, Eq, PartialEq)]
pub enum EdgeVerticesError {
    OriginMissingError,
    DestinationMissingError,
}

#[derive(Debug, Eq, PartialEq)]
pub enum MergeVerticesError {
    V1MissingError,
    V2MissingError,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TraverseEdgeError {
    VertexMissingError,
    EdgeMissingError,
}

/*
 * graph
 */

#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
struct SerializableGraph<V, E>
where
    V: Eq + Hash,
    E: Eq,
{
    vertex_map: HashMap<Rc<V>, VId>,
    adjacency_map: HashMap<VId, HashMap<VId, Rc<E>>>,
    next_id: VId,
}

#[derive(Debug, Clone, Eq, PartialEq, Deserialize)]
struct DeserializableGraph<V, E>
where
    V: Eq + Hash,
    E: Eq,
{
    vertex_map: HashMap<Rc<V>, VId>,
    adjacency_map: HashMap<VId, HashMap<VId, E>>,
    next_id: VId,
}

type VId = usize;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Graph<V, E> 
where
    V: Eq + Hash,
    E: Eq,
{
    id_vertex_map: HashMap<VId, Rc<V>>,
    vertex_id_map: HashMap<Rc<V>, VId>,
    adjacency_map: HashMap<VId, HashMap<VId, E>>,
    next_id: VId,
}

impl<V, E> Serialize for Graph<V, E>
where
    V: Eq + Hash + Serialize,
    E: Eq + Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let s_graph = SerializableGraph {
            vertex_map: self.vertex_id_map.clone(),
            adjacency_map: self.adjacency_map.iter().map(|(&origin_vid, adj)| (origin_vid, adj.iter().map(|(&dest_vid, edge)| (dest_vid, Rc::new(edge))).collect())).collect(),
            next_id: self.next_id,
        };

        s_graph.serialize(serializer)
    }
}

impl<'de, V, E> Deserialize<'de> for Graph<V, E>
where
    V: Eq + Hash + Deserialize<'de>,
    E: Eq + Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let d_graph: DeserializableGraph<V, E> = DeserializableGraph::deserialize(deserializer)?;

        let graph: Graph<V, E> = Graph {
            id_vertex_map: d_graph.vertex_map.iter().map(|(vert, &vid)| (vid, vert.clone())).collect(),
            vertex_id_map: d_graph.vertex_map.clone(),
            adjacency_map: d_graph.adjacency_map,
            next_id: d_graph.next_id,
        };

        Ok(graph)
    }
}

impl<V, E> fmt::Display for Graph<V, E>
where
    V: Eq + Hash,
    E: Eq,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "graph{{ vertex_count: {} }}", self.id_vertex_map.len())
    }
}

impl<V, E> Index<(&V, &V)> for Graph<V, E> 
where
    V: Eq + Hash,
    E: Eq,
{
    type Output = E;

    fn index(&self, index: (&V, &V)) -> &Self::Output {
        self.get_edge_label(index.0, index.1).unwrap().unwrap()
    }
}

impl<V, E> IndexMut<(&V, &V)> for Graph<V, E> 
where
    V: Eq + Hash,
    E: Eq,
{
    fn index_mut(&mut self, index: (&V, &V)) -> &mut Self::Output {
        self.get_edge_label_mut(index.0, index.1).unwrap().unwrap()
    }
}

impl<V, E> Graph<V, E> 
where 
    V: Eq + Hash,
    E: Eq,
{
    pub fn new() -> Self {
        Self {
            id_vertex_map: HashMap::new(),
            vertex_id_map: HashMap::new(),
            adjacency_map: HashMap::new(),

            next_id: 0,
        }
    }

    fn id_to_vertex(&self, id: &VId) -> Option<&V> {
        self.id_vertex_map.get(id).map(|e| e.deref())
    }

    fn vertex_to_id(&self, label: &V) -> Option<VId> {
        self.vertex_id_map.get(label).copied()
    }

    pub fn push_vertex(&mut self, label: V) -> Result<(), VertexExistsError> {
        let rc_label = Rc::new(label);
        match self.vertex_id_map.entry(rc_label.clone()) {
            std::collections::hash_map::Entry::Occupied(_) => { 
                return Err(VertexExistsError); 
            },
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(self.next_id);
            }
        };
        self.id_vertex_map.insert(self.next_id, rc_label);
        self.adjacency_map.insert(self.next_id, HashMap::new());
        self.next_id += 1;
        Ok(())
    }

    pub fn push_edge(&mut self, label: E, origin_label: &V, dest_label: &V) -> Result<(), EdgePushError> {
        let origin_id = match self.vertex_to_id(origin_label) {
            Some(id) => id,
            None => {
                return Err(EdgePushError::OriginMissingError);
            }
        };
        let dest_id = match self.vertex_to_id(dest_label) {
            Some(id) => id,
            None => {
                return Err(EdgePushError::DestinationMissingError);
            }
        };

        match self.adjacency_map.get_mut(&origin_id).unwrap().entry(dest_id) {
            std::collections::hash_map::Entry::Occupied(_) => Err(EdgePushError::EdgeExistsError),
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(label);
                Ok(())
            }
        }
    }

    pub fn ensure_vertex(&mut self, label: V) {
        let _ = self.push_vertex(label);
    }

    pub fn ensure_edge(&mut self, label: E, origin_label: &V, dest_label: &V) {
        let _ = self.push_edge(label, origin_label, dest_label);
    }

    pub fn contains_vertex(&self, label: &V) -> bool {
        self.vertex_id_map.contains_key(label)
    }

    pub fn contains_edge(&self, origin_label: &V, dest_label: &V) -> Result<bool, EdgeVerticesError> {
        let origin_id = match self.vertex_to_id(origin_label) {
            Some(id) => id,
            None => {
                return Err(EdgeVerticesError::OriginMissingError);
            }
        };
        let dest_id = match self.vertex_to_id(dest_label) {
            Some(id) => id,
            None => {
                return Err(EdgeVerticesError::DestinationMissingError);
            }
        };

        Ok(self.adjacency_map.get(&origin_id).unwrap().contains_key(&dest_id))
    }

    pub fn get_edge_label(&self, origin_label: &V, dest_label: &V) -> Result<Option<&E>, EdgeVerticesError> {
        let origin_id = match self.vertex_to_id(origin_label) {
            Some(id) => id,
            None => {
                return Err(EdgeVerticesError::OriginMissingError);
            }
        };
        let dest_id = match self.vertex_to_id(dest_label) {
            Some(id) => id,
            None => {
                return Err(EdgeVerticesError::DestinationMissingError);
            }
        };

        match self.adjacency_map.get(&origin_id) {
            Some(origin_value) => Ok(origin_value.get(&dest_id)),
            None => Ok(None)
        }
    }

    pub fn get_edge_label_mut(&mut self, origin_label: &V, dest_label: &V) -> Result<Option<&mut E>, EdgeGetError> {
        let origin_id = match self.vertex_to_id(origin_label) {
            Some(id) => id,
            None => {
                return Err(EdgeGetError::OriginMissingError);
            }
        };
        let dest_id = match self.vertex_to_id(dest_label) {
            Some(id) => id,
            None => {
                return Err(EdgeGetError::DestinationMissingError);
            }
        };

        match self.adjacency_map.get_mut(&origin_id) {
            Some(adj) => {
                Ok(adj.get_mut(&dest_id))
            },
            None => {
                Err(EdgeGetError::EdgeMissingError)
            }
        }
    }

    pub fn remove_vertex(&mut self, label: &V) -> Result<(), VertexMissingError> {
        let id = match self.vertex_id_map.remove(label) {
            Some(id) => id,
            None => {
                return Err(VertexMissingError);
            }
        };
        self.id_vertex_map.remove(&id);

        self.adjacency_map.remove(&id).unwrap();
        for (_, adj) in self.adjacency_map.iter_mut() {
            adj.remove(&id);
        }
        Ok(())
    }

    pub fn remove_edge(&mut self, origin_label: &V, dest_label: &V) -> Result<(), EdgeRemoveError> {
        let origin_id = match self.vertex_to_id(origin_label) {
            Some(id) => id,
            None => {
                return Err(EdgeRemoveError::OriginMissingError);
            }
        };
        let dest_id = match self.vertex_to_id(dest_label) {
            Some(id) => id,
            None => {
                return Err(EdgeRemoveError::DestinationMissingError);
            }
        };

        match self.adjacency_map.get_mut(&origin_id).unwrap().remove(&dest_id) {
            Some(_) => Ok(()),
            None => Err(EdgeRemoveError::EdgeMissingError)
        }
    }

    pub fn traverse_edge(&self, label: &V, edge: &E) -> Result<&V, TraverseEdgeError> {
        let id = match self.vertex_to_id(label) {
            Some(v) => v,
            None => {
                return Err(TraverseEdgeError::VertexMissingError);
            }
        };
        for (dest_vid, e) in self.adjacency_map.get(&id).unwrap().iter() {
            if e == edge {
                return Ok(self.id_to_vertex(dest_vid).unwrap());
            }
        }
        Err(TraverseEdgeError::EdgeMissingError)
    }

    pub fn get_in_degree(&self, label: &V) -> Result<usize, VertexMissingError> {
        let id = match self.vertex_to_id(label) {
            Some(id) => id,
            None => {
                return Err(VertexMissingError);
            }
        };

        let mut count = 0;
        for (_, adj) in self.adjacency_map.iter() {
            if adj.contains_key(&id) {
                count += 1;
            }
        }
        Ok(count)
    }

    pub fn get_out_degree(&self, label: &V) -> Result<usize, VertexMissingError> {
        let id = match self.vertex_to_id(label) {
            Some(id) => id,
            None => {
                return Err(VertexMissingError);
            }
        };

        match self.adjacency_map.get(&id) {
            Some(adj) => {
                Ok(adj.len())
            },
            None => Err(VertexMissingError)
        }
    }

    pub fn merge_vertices(&mut self, v1: &V, v2: &V) -> Result<(), MergeVerticesError> {
        let id1 = match self.vertex_to_id(v1) {
            Some(id) => id,
            None => {
                return Err(MergeVerticesError::V1MissingError);
            }
        };
        let id2 = match self.vertex_id_map.remove(v2) {
            Some(id) => id,
            None => {
                return Err(MergeVerticesError::V2MissingError);
            }
        };
        self.id_vertex_map.remove(&id2);

        for (_, adj) in self.adjacency_map.iter_mut() {
            match adj.remove(&id2) {
                Some(value) => {
                    adj.insert(id1, value);
                },
                None => {},
            }
        }
        match self.adjacency_map.remove(&id2) {
            Some(value) => {
                match self.adjacency_map.get_mut(&id1) {
                    Some(adj) => {
                        adj.extend(value);
                    },
                    None => {
                        return Err(MergeVerticesError::V1MissingError);
                    }
                }
                Ok(())
            },
            None => Err(MergeVerticesError::V2MissingError)
        }
    }

    pub fn iter_vertices(&self) -> impl Iterator<Item = &V> + '_ {
        self.adjacency_map.keys().map(
            |&id| self.id_to_vertex(&id).unwrap()
        )
    }

    pub fn iter_sources<'b>(&'b self, label: &'b V) -> Result<impl Iterator<Item = (&E, &V)> + 'b, VertexMissingError> {
        let id = match self.vertex_to_id(label) {
            Some(id) => id,
            None => {
                return Err(VertexMissingError);
            }
        };

        Ok(self.adjacency_map.iter().flat_map(
            move |(origin_id, adj)| {
                adj.iter().filter(
                    move |(&dest_id, _)| dest_id == id
                ).map(
                    move |(_, e)| (e, self.id_to_vertex(origin_id).unwrap())
                )
            }
        ))
    }

    pub fn iter_targets(&self, label: &V) -> Result<impl Iterator<Item = (&E, &V)> + '_, VertexMissingError> {
        let id = match self.vertex_to_id(label) {
            Some(id) => id,
            None => {
                return Err(VertexMissingError);
            }
        };

        Ok(self.adjacency_map.get(&id).unwrap().iter().map(
            |(vid, e)| (e, self.id_to_vertex(vid).unwrap())
        ))
    }

    pub fn assert_invariant(&self) {
        assert_eq!(self.id_vertex_map.len() == self.vertex_id_map.len() && self.vertex_id_map.len() == self.adjacency_map.len(), true);
        self.id_vertex_map.values().for_each(|vert| {
            assert_eq!(self.vertex_id_map.contains_key(vert), true);
        });
        self.vertex_id_map.values().for_each(|vid| {
            assert_eq!(self.id_vertex_map.contains_key(vid), true);
        });
        self.id_vertex_map.keys().for_each(|vid| {
            assert_eq!(self.adjacency_map.contains_key(vid), true);
        });
        self.adjacency_map.values().for_each(|adj| {
            adj.keys().for_each(|dest_id| {
                assert_eq!(self.id_vertex_map.contains_key(dest_id), true);
            });
        });
        self.id_vertex_map.values().for_each(|vert| {
            assert_eq!(Rc::strong_count(vert), 2);
        });
        self.vertex_id_map.keys().for_each(|vert| {
            assert_eq!(Rc::strong_count(vert), 2);
        });
    }

    pub fn save_to_file(&self, filename: &str) -> Result<(), std::io::Error> 
    where
        Self: Serialize,
    {
        let mut file = File::create(filename)?;
        file.write_all(serde_json::to_string(&self)?.as_bytes())?;
        Ok(())
    }

    pub fn load_from_file(filename: &str) -> Result<Self, std::io::Error>
    where
        V: DeserializeOwned,
        Self: DeserializeOwned + Sized,
    {
        let serialized = fs::read_to_string(filename)?;
        let graph = serde_json::from_str(&serialized)?;
        Ok(graph)
    }
}

/* search */
impl<V, E> Graph<V, E> 
where
    V: Eq + Hash,
    E: Eq,
{
    /* dfs search */
    pub fn vertex_dfs_forward(&self, origin_label: &V) -> impl Iterator<Item = &V> {
        // this searches vertices via dfs. notice how vertices being searched does not mean all
        // edges are searched.
        let next: fn(&VId, &HashMap<VId, HashMap<VId, ()>>) -> Vec<(VId, ())> = |vid, adjacency_map| {
            adjacency_map.get(vid).unwrap().keys().map(|&v| (v, ())).collect()
        };
        let adjacency_map = self.adjacency_map.iter().map(|(&origin_vid, adj)| (origin_vid, adj.iter().map(|(&dest_vid, _)| (dest_vid, ())).collect())).collect();
        
        let dfs = DFS::new(self.vertex_to_id(origin_label).unwrap(), next, adjacency_map);
        dfs.map(|(vid, _)| (self.id_to_vertex(&vid).unwrap()))
    }

    pub fn edge_dfs_forward(&self, origin_label: &V) -> impl Iterator<Item = (&V, Option<&V>)> {
        let next: fn(&VId, &HashMap<VId, HashMap<VId, VId>>) -> Vec<(VId, VId)> = |origin_vid, adjacency_map| {
            adjacency_map.get(origin_vid).unwrap().keys().map(|&dest_vid| (dest_vid, *origin_vid)).collect()
        };
        let adjacency_map = self.adjacency_map.iter().map(|(&origin_vid, adj)| (origin_vid, adj.iter().map(|(&dest_vid, _)| (dest_vid, origin_vid)).collect())).collect();
        
        let dfs = DFS::new(self.vertex_to_id(origin_label).unwrap(), next, adjacency_map);
        dfs.map(|(origin_vid, op_edge_vids)| (self.id_to_vertex(&origin_vid).unwrap(), op_edge_vids.map(|dest_vid| self.id_to_vertex(&dest_vid).unwrap())))
    }

    pub fn vertex_dfs_backward(&self, origin_label: &V) -> impl Iterator<Item = &V> {
        // this searches vertices via dfs. notice how vertices being searched does not mean all
        // edges are searched.
        let next: fn(&VId, &HashMap<VId, HashMap<VId, ()>>) -> Vec<(VId, ())> = |vid, adjacency_map| {
            adjacency_map.get(vid).unwrap().keys().map(|&v| (v, ())).collect()
        };

        // reverse the adjacency map for backwards traversal
        let mut adjacency_map: HashMap<VId, HashMap<VId, ()>> = HashMap::new();
        for (&origin_vid, adj) in self.adjacency_map.iter() {
            for (&dest_vid, _) in adj.iter() {
                match adjacency_map.entry(dest_vid) {
                    std::collections::hash_map::Entry::Occupied(mut val) => { 
                        val.get_mut().insert(origin_vid, ());
                    },
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        let mut map = HashMap::new();
                        map.insert(origin_vid, ());
                        entry.insert(map);
                    }
                }
            }
        }
        
        let dfs = DFS::new(self.vertex_to_id(origin_label).unwrap(), next, adjacency_map);
        dfs.map(|(vid, _)| (self.id_to_vertex(&vid).unwrap()))
    }

    pub fn edge_dfs_backward(&self, origin_label: &V) -> impl Iterator<Item = (&V, Option<&V>)> {
        // this searches vertices via dfs. notice how vertices being searched does not mean all
        // edges are searched.
        let next: fn(&VId, &HashMap<VId, HashMap<VId, VId>>) -> Vec<(VId, VId)> = |vid, adjacency_map| {
            adjacency_map.get(vid).unwrap().keys().map(|&v| (v, *vid)).collect()
        };

        // reverse the adjacency map for backwards traversal
        let mut adjacency_map: HashMap<VId, HashMap<VId, VId>> = HashMap::new();
        for (&origin_vid, adj) in self.adjacency_map.iter() {
            for (&dest_vid, _) in adj.iter() {
                match adjacency_map.entry(dest_vid) {
                    std::collections::hash_map::Entry::Occupied(mut val) => { 
                        val.get_mut().insert(origin_vid, dest_vid);
                    },
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        let mut map = HashMap::new();
                        map.insert(origin_vid, dest_vid);
                        entry.insert(map);
                    }
                }
            }
        }

        let dfs = DFS::new(self.vertex_to_id(origin_label).unwrap(), next, adjacency_map);
        dfs.map(|(origin_vid, op_edge_vids)| (self.id_to_vertex(&origin_vid).unwrap(), op_edge_vids.map(|dest_vid| self.id_to_vertex(&dest_vid).unwrap())))
    }

    /* bfs search */
    pub fn vertex_bfs_forward(&self, origin_label: &V) -> impl Iterator<Item = &V> {
        // this searches vertices via dfs. notice how vertices being searched does not mean all
        // edges are searched.
        let next: fn(&VId, &HashMap<VId, HashMap<VId, ()>>) -> Vec<(VId, ())> = |vid, adjacency_map| {
            adjacency_map.get(vid).unwrap().keys().map(|&v| (v, ())).collect()
        };
        let adjacency_map = self.adjacency_map.iter().map(|(&origin_vid, adj)| (origin_vid, adj.iter().map(|(&dest_vid, _)| (dest_vid, ())).collect())).collect();
        
        let bfs = BFS::new(self.vertex_to_id(origin_label).unwrap(), next, adjacency_map);
        bfs.map(|(vid, _)| (self.id_to_vertex(&vid).unwrap()))
    }

    pub fn edge_bfs_forward(&self, origin_label: &V) -> impl Iterator<Item = (&V, Option<&V>)> {
        let next: fn(&VId, &HashMap<VId, HashMap<VId, VId>>) -> Vec<(VId, VId)> = |origin_vid, adjacency_map| {
            adjacency_map.get(origin_vid).unwrap().keys().map(|&dest_vid| (dest_vid, *origin_vid)).collect()
        };
        let adjacency_map = self.adjacency_map.iter().map(|(&origin_vid, adj)| (origin_vid, adj.iter().map(|(&dest_vid, _)| (dest_vid, origin_vid)).collect())).collect();
        
        let bfs = BFS::new(self.vertex_to_id(origin_label).unwrap(), next, adjacency_map);
        bfs.map(|(origin_vid, op_edge_vids)| (self.id_to_vertex(&origin_vid).unwrap(), op_edge_vids.map(|dest_vid| self.id_to_vertex(&dest_vid).unwrap())))
    }

    pub fn vertex_bfs_backward(&self, origin_label: &V) -> impl Iterator<Item = &V> {
        // this searches vertices via dfs. notice how vertices being searched does not mean all
        // edges are searched.
        let next: fn(&VId, &HashMap<VId, HashMap<VId, ()>>) -> Vec<(VId, ())> = |vid, adjacency_map| {
            adjacency_map.get(vid).unwrap().keys().map(|&v| (v, ())).collect()
        };

        // reverse the adjacency map for backwards traversal
        let mut adjacency_map: HashMap<VId, HashMap<VId, ()>> = HashMap::new();
        for (&origin_vid, adj) in self.adjacency_map.iter() {
            for (&dest_vid, _) in adj.iter() {
                match adjacency_map.entry(dest_vid) {
                    std::collections::hash_map::Entry::Occupied(mut val) => { 
                        val.get_mut().insert(origin_vid, ());
                    },
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        let mut map = HashMap::new();
                        map.insert(origin_vid, ());
                        entry.insert(map);
                    }
                }
            }
        }
        
        let bfs = BFS::new(self.vertex_to_id(origin_label).unwrap(), next, adjacency_map);
        bfs.map(|(vid, _)| (self.id_to_vertex(&vid).unwrap()))
    }

    pub fn edge_bfs_backward(&self, origin_label: &V) -> impl Iterator<Item = (&V, Option<&V>)> {
        // this searches vertices via dfs. notice how vertices being searched does not mean all
        // edges are searched.
        let next: fn(&VId, &HashMap<VId, HashMap<VId, VId>>) -> Vec<(VId, VId)> = |vid, adjacency_map| {
            adjacency_map.get(vid).unwrap().keys().map(|&v| (v, *vid)).collect()
        };

        // reverse the adjacency map for backwards traversal
        let mut adjacency_map: HashMap<VId, HashMap<VId, VId>> = HashMap::new();
        for (&origin_vid, adj) in self.adjacency_map.iter() {
            for (&dest_vid, _) in adj.iter() {
                match adjacency_map.entry(dest_vid) {
                    std::collections::hash_map::Entry::Occupied(mut val) => { 
                        val.get_mut().insert(origin_vid, dest_vid);
                    },
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        let mut map = HashMap::new();
                        map.insert(origin_vid, dest_vid);
                        entry.insert(map);
                    }
                }
            }
        }

        let bfs = BFS::new(self.vertex_to_id(origin_label).unwrap(), next, adjacency_map);
        bfs.map(|(origin_vid, op_edge_vids)| (self.id_to_vertex(&origin_vid).unwrap(), op_edge_vids.map(|dest_vid| self.id_to_vertex(&dest_vid).unwrap())))
    }
}

impl<V, E> Reachable for Graph<V, E> 
where
    V: Eq + Hash + std::fmt::Display,
    E: Eq + std::fmt::Display,
{
    type T = V;

    fn can_reach(&self, start: &Self::T, end: &Self::T) -> bool {
        for v in self.vertex_dfs_forward(start) {
            if v == end {
                return true;
            }
        }
        false
    }

    fn distance(&self, start: &Self::T, end: &Self::T) -> Option<usize> {
        let mut has = false;
        let mut vertices: HashMap<&Self::T, Option<&Self::T>> = HashMap::new();
        for (dest_label, origin_label) in self.edge_bfs_forward(start) {
            vertices.insert(dest_label, origin_label);
            if dest_label == end {
                has = true;
                break;
            }
        }
        if !has {
            return None
        }

        let mut dist: usize = 0;
        let mut curr = vertices[end];
        while curr != None {
            curr = vertices[curr.unwrap()];
            dist += 1;
        }
        Some(dist)
    }
}

impl<V, E> Summary for Graph<V, E>
where
    V: Eq + Hash + ToString,
    E: Eq + ToString,
{
    fn summarize(&self, lines: usize) -> String {
        if lines <= 0 {
            return String::from("");
        } else if lines <= 3 {
            return format!("graph{{ vertex_count: {} }}", self.id_vertex_map.len());
        } else {
            let mut ln = 0;
            let mut unfinished = false;
            let mut result = String::new();
            result += "graph {\n";
            for v in self.iter_vertices() {
                if ln >= lines - 3 {
                    unfinished = true;
                    break;
                }
                result += &(String::from("  ") + &v.to_string());
                result += " => ";
                self.iter_targets(v).unwrap().for_each(|(e, t)| {
                    result += &t.to_string();
                    result += "(";
                    result += &e.to_string();
                    result += ") ";
                });
                result += "\n";
                ln += 1;
            };
            if unfinished {
                result += "...\n";
            }
            result += "}";
            return result;
        }
    }
}

#[test]
fn test_graph() {
    let graph: &mut Graph<i32, String> = &mut Graph::new();

    /*
     * test add
     */
    assert_eq!(graph.push_vertex(1), Ok(()));
    assert_eq!(graph.push_vertex(1), Err(VertexExistsError));
    assert_eq!(graph.push_vertex(3), Ok(()));
    assert_eq!(graph.push_vertex(7), Ok(()));
    assert_eq!(graph.push_vertex(8), Ok(()));
    assert_eq!(graph.push_vertex(9), Ok(()));
    assert_eq!(graph.push_vertex(7), Err(VertexExistsError));

    assert_eq!(graph.push_edge(String::from("1-3 edge"), &1, &3), Ok(()));
    assert_eq!(graph.push_edge(String::from("1-7 edge"), &1, &7), Ok(()));
    assert_eq!(graph.push_edge(String::from("7-1 edge"), &7, &1), Ok(()));
    assert_eq!(graph.push_edge(String::from("3-7 edge"), &3, &7), Ok(()));
    assert_eq!(graph.push_edge(String::from("3-8 edge"), &3, &8), Ok(()));
    assert_eq!(graph.push_edge(String::from("8-3 edge"), &8, &3), Ok(()));
    assert_eq!(graph.push_edge(String::from("9-3 edge"), &9, &3), Ok(()));
    assert_eq!(graph.push_edge(String::from("3-9 edge"), &3, &9), Ok(()));
    assert_eq!(graph.push_edge(String::from("9-7 edge"), &9, &7), Ok(()));
    assert_eq!(graph.push_edge(String::from("1-1 self"), &1, &1), Ok(()));
    assert_eq!(graph.push_edge(String::from("another 1-7 edge"), &1, &7), Err(EdgePushError::EdgeExistsError));
    assert_eq!(graph.push_edge(String::from("2-7 edge"), &2, &7), Err(EdgePushError::OriginMissingError));
    assert_eq!(graph.push_edge(String::from("7-2 edge"), &7, &2), Err(EdgePushError::DestinationMissingError));

    /*
     * test contains
     */
    assert_eq!(graph.contains_vertex(&1), true);
    assert_eq!(graph.contains_vertex(&2), false);
    assert_eq!(graph.contains_vertex(&3), true);
    assert_eq!(graph.contains_vertex(&7), true);

    assert_eq!(graph.contains_edge(&1, &7), Ok(true));
    assert_eq!(graph.contains_edge(&7, &1), Ok(true));
    assert_eq!(graph.contains_edge(&3, &1), Ok(false));
    assert_eq!(graph.contains_edge(&1, &6), Err(EdgeVerticesError::DestinationMissingError));
    assert_eq!(graph.contains_edge(&2, &7), Err(EdgeVerticesError::OriginMissingError));

    assert_eq!(graph.get_edge_label(&1, &7), Ok(Some(&String::from("1-7 edge"))));
    assert_eq!(graph.get_edge_label(&7, &1), Ok(Some(&String::from("7-1 edge"))));
    assert_eq!(graph.get_edge_label(&3, &1), Ok(None));
    assert_eq!(graph.get_edge_label(&1, &6), Err(EdgeVerticesError::DestinationMissingError));
    assert_eq!(graph.get_edge_label(&2, &7), Err(EdgeVerticesError::OriginMissingError));

    /*
     * test degrees
     */
    assert_eq!(graph.get_in_degree(&1), Ok(2));
    assert_eq!(graph.get_in_degree(&7), Ok(3));
    assert_eq!(graph.get_in_degree(&2), Err(VertexMissingError));
    assert_eq!(graph.get_out_degree(&1), Ok(3));
    assert_eq!(graph.get_out_degree(&7), Ok(1));
    assert_eq!(graph.get_out_degree(&2), Err(VertexMissingError));

    /* 
     * test remove
     */
    assert_eq!(graph.remove_vertex(&1), Ok(()));
    assert_eq!(graph.contains_vertex(&1), false);
    assert_eq!(graph.contains_edge(&1, &7), Err(EdgeVerticesError::OriginMissingError));
    assert_eq!(graph.contains_edge(&7, &1), Err(EdgeVerticesError::DestinationMissingError));
    assert_eq!(graph.contains_edge(&3, &1), Err(EdgeVerticesError::DestinationMissingError));

    assert_eq!(graph.remove_edge(&3, &8), Ok(()));
    assert_eq!(graph.contains_edge(&3, &8), Ok(false));
    assert_eq!(graph.contains_edge(&8, &3), Ok(true));
    assert_eq!(graph.contains_edge(&3, &7), Ok(true));

    /*
     * test iter
     */
    graph.iter_vertices().for_each(|v| {
        graph.iter_sources(v).unwrap().for_each(|(e, s)| {
            assert_eq!(graph.get_edge_label(s, v), Ok(Some(e)));
        });
        graph.iter_targets(v).unwrap().for_each(|(e, t)| {
            assert_eq!(graph.get_edge_label(v, t), Ok(Some(e)));
        });
        assert_eq!(graph.contains_vertex(v), true);
    });

    /*
     * test merge
     */
    assert_eq!(graph.merge_vertices(&9, &3), Ok(()));
    assert_eq!(graph.contains_edge(&8, &9), Ok(true));
    assert_eq!(graph.merge_vertices(&2, &8), Err(MergeVerticesError::V1MissingError));
    assert_eq!(graph.merge_vertices(&8, &2), Err(MergeVerticesError::V2MissingError));

    let serialized_graph = serde_json::to_string(&graph).unwrap();
    let deserialized_graph: Graph<i32, String> = serde_json::from_str(&serialized_graph).unwrap();
    assert_eq!(graph, &deserialized_graph);

    graph.assert_invariant();
    deserialized_graph.assert_invariant();

    let _ = graph.save_to_file("graph.txt");
    let file_graph: Graph<i32, String> = Graph::load_from_file("graph.txt").unwrap();
    assert_eq!(graph, &file_graph);

    graph.assert_invariant();
    file_graph.assert_invariant();

    fs::remove_file("graph.txt").unwrap();
}
