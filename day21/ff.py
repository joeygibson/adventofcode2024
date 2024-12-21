from collections import deque

def bfs(graph, start):
    # Initialize a queue for BFS and a set to track visited nodes
    queue = deque([start])
    visited = set()

    # Mark the starting node as visited
    visited.add(start)

    while queue:
        # Dequeue the next node to process
        current = queue.popleft()
        print(current)  # Process the current node (e.g., print it)

        # Iterate through all neighbors of the current node
        for neighbor in graph[current]:
            if neighbor not in visited:
                visited.add(neighbor)  # Mark neighbor as visited
                queue.append(neighbor)  # Enqueue the neighbor

# Example graph represented as a hash table (dictionary)
graph = {
    "A": ["B", "C", "D"],
    "B": ["A", "E"],
    "C": ["A", "F"],
    "D": ["A"],
    "E": ["B"],
    "F": ["C"]
}

# Perform BFS starting from node 'A'
bfs(graph, "A")

