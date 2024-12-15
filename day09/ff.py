def main():
    with open('input1.txt', 'r') as file:
        disk_map = file.read()
        disk_map = disk_map.rstrip('\n')

    file_blocks, free_blocks = getBlockLists(disk_map)
    blocks = createBlocks(file_blocks, free_blocks)
    blocks = moveFiles(blocks)
    result = checkSum(blocks)
    print(result)

# Splits the disk map into 2 int lists representing the file blocks and the free space blocks
def getBlockLists(disk_map):
    free_blocks = []
    file_blocks = []

    for i in range(len(disk_map)):
        if i % 2 == 0: # even indices are file blocks and odd are free blocks
            file_blocks.append(int(disk_map[i]))
        else:
            free_blocks.append(int(disk_map[i]))

    return file_blocks, free_blocks

# Returns a single list representing the block representation of the original puzzle input
def createBlocks(file_blocks, free_blocks):
    block_list = []
    for i in range(len(file_blocks)):
        block_list.append([str(i)] * file_blocks[i])
        if i < len(free_blocks):  # free_blocks list has 1 less element than file_blocks
            block_list.append(["."] * free_blocks[i])
    return block_list

# Moves file blocks into valid memory blocks and returns the new list
def moveFiles(block_list):
    # Move files by descending ID
    for file_index in range(len(block_list) - 1, -1, -1):
        file_block = block_list[file_index]

        # Skip free blocks
        if all(char == "." for char in file_block): continue

        file_len = len(file_block)

        # Find a block of free memory that can fit the file
        for i in range(file_index): # Only check free_blocks that appear before the file block
            if "." in block_list[i] and len(block_list[i]) >= file_len:
                # Move the file into the free block
                free_len = len(block_list[i])
                block_list[i] = file_block
                block_list[file_index] = ["."] * file_len

                # Handle remaining free space
                remaining_mem = free_len - file_len
                if remaining_mem > 0:
                    # Insert the remaining memory as a new free block
                    block_list.insert(i + 1, ["."] * remaining_mem)
                break # move on to the next file

    # Expand every file_block so every ID occupies its own index and every free_block so every "." occupies its own index
    expanded_block_list = []
    for block in block_list:
        expanded_block_list.extend(block)
    return expanded_block_list

# Multiplies file ID by its index and returns the sum of these values for all file blocks
def checkSum(block_list):
    result = 0
    for i, block in enumerate(block_list):
        if block != ".":
            result += i * int(block)
    return result

main()
