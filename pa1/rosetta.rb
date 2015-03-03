
def main

  content = STDIN.readlines
  graph = Hash.new
  indeg = Hash.new
  order = Array.new
  
  # dicard \n at each line
  for i in 0...content.size
    content[i] = content[i].gsub("\n","").gsub("\r","")
  end

  # initialize
  content.each do |line|
    if !indeg.has_key?(line)
      indeg[line] = 0
    end
    if !graph.has_key?(line)
      graph[line] = Array.new
    end
  end

  # construct graph
  i = 0
  while i < content.size()
    graph[content[i+1]].push(content[i])
    indeg[content[i]] += 1
    i += 2
  end

  # topological sort
  while true
    next_node = nil
    indeg.each_key do |node|
      if indeg[node] == 0 && (next_node.nil? || node < next_node)
        next_node = node
      end
    end
    
    if next_node.nil?
      break
    end
    
    order.push(next_node)
    indeg[next_node] = -1
    graph[next_node].each do |node|
      indeg[node] -= 1
    end
  end

  if order.size != indeg.size
    printf("cycle\n")
  else
    order.each do |node|
      printf("%s\n", node)
    end
  end

end


if __FILE__ == $0
  main
end

