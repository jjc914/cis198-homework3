import networkx as nx
import matplotlib.pyplot as plt

def read_data_from_file(filename):
    nodes_data = []
    edges_data = []

    with open(filename, 'r') as file:
        lines = file.readlines()

        # Splitting lines into nodes and edges data
        for line in lines:
            parts = line.strip().split(',')
            
            # Checking if it's a node or edge data based on the number of elements in parts
            if parts[0] == 'v':  # Node data
                uid, node_type, content = parts[1:]
                nodes_data.append((int(uid), node_type.strip(), content.strip()))
            elif parts[0] == 'e':  # Edge data
                src_uid, tgt_uid, edge_label = map(int, parts[1:])
                edges_data.append((src_uid, tgt_uid, edge_label))
    
    return nodes_data, edges_data

# Your data lists
nodes_data, edges_data = read_data_from_file("temp_tree_visualize_data")

# Create a directed graph
G = nx.DiGraph()

# Create node_labels dictionary to store labels
node_labels = {}

# Add nodes with their labels based on content availability
for uid, node_type, content in nodes_data:
    G.add_node(uid)
    label = content if content else node_type
    node_labels[uid] = label

# Add edges and assign edge labels
for src_uid, tgt_uid, edge_label in edges_data:
    G.add_edge(src_uid, tgt_uid, label=str(edge_label))

# Set the layout to 'dot' for top-down representation
pos = nx.drawing.nx_pydot.graphviz_layout(G, prog='dot')

# Display the graph with labels
plt.figure(figsize=(8, 8))
nx.draw(G, pos, with_labels=True, labels=node_labels, node_color='skyblue', node_size=700, font_size=8, font_color='black', font_weight='bold', arrows=True)
edge_labels = nx.get_edge_attributes(G, 'label')
nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels, font_color='red')
plt.title("Top-Down Graph Visualization with Content or Type as Labels")
plt.show()
