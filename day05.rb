def real_input
  File.read('day05/input.txt').split.map(&:to_i)
end

def test_input
  [0, 3, 0, 1, -3]
end

def num_to_finish maze
  index = 0
  count = 0
  while index >= 0 && index < maze.length
    index_val = maze[index]
    if index_val >= 3
      maze[index] -= 1
    else
      maze[index] += 1
    end
    index = index + index_val
    count += 1
  end
  count
end
