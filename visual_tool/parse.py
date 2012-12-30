import pprint
import re
import xmlrpclib
import sys
import math

import ubigraph
U = ubigraph.Ubigraph()
U.clear()

redSphere = U.newVertexStyle(shape="sphere", color="#ff0000")
wideEdge = U.newEdgeStyle(color="#ff9900", stroke="solid")#, spline=True)
parentChildEdge = U.newEdgeStyle(color="#0099ff", arrow=True,
    arrow_position=1.0, stroke="dashed")
#parentChildEdge = U.newEdgeStyle(color="#0099ff", arrow=True,
#    arrow_position=0.5, arrow_length=15.0, arrow_radius=13.0, spline=True, 
#    stroke="dashed", width=3.0)

pp = pprint.PrettyPrinter(indent=4)
vertices = {}
edges = {}
actors_birth = {}
actors_lifespan = {}
actors_parent = {}
vertex_num = 0

if len(sys.argv) == 1:
  print "Please specify the name of SystemTap generated file from\
  ./traces/ folder"
  exit(0)

f = open("traces/"+sys.argv[1], "r")
all_lines = f.readlines()

start_exec_time = int(all_lines[0].split()[-1])
end_exec_time = int(all_lines[-1].split()[-1]) - start_exec_time

for line in all_lines:
  tokens = line.split()
  if tokens[0] == "spawn:":
    vertex_num += 1
    if (vertex_num > 28 or vertex_num == 3):
      actors_birth[tokens[1]] = int(tokens[-1]) - start_exec_time
      vertices[tokens[1]] = 0
      actors_parent[tokens[1]] = tokens[4]
  elif tokens[0] == "send:" and len(tokens) == 8:
    if tokens[1] in vertices:
      if tokens[3] in vertices:
        sender = tokens[1]
        receiver = tokens[3]
        if (sender, receiver) in edges:
          edges[(sender, receiver)] += 1
        else:
          edges[(sender, receiver)] = 1
  elif tokens[0] == "exit:":
    if tokens[1] in vertices:
      actors_lifespan[tokens[1]] = (int(tokens[-1]) - start_exec_time) - actors_birth[tokens[1]]
#    if tokens[1] == "<0.2.0>":
#      parent_exit_time = actors_lifespan["<0.2.0>"]

## Draw vertices
for vertex in vertices.keys():
#  if vertex not in actors_lifespan.keys():
#    print "hey, what about me: %s?" % vertex
#    actors_lifespan[vertex] = parent_exit_time - actors_birth[vertex]
  vertex_size = 1. / math.log(float(end_exec_time) / float(actors_lifespan[vertex]))
  vertices[vertex] = U.newVertex(style=redSphere, label=vertex, size=vertex_size)

## Draw edges for parent/child relationship
for vertex in actors_parent.keys():
  if actors_parent[vertex] in vertices.keys():
    vertex_id =  vertices[vertex]
    parent_id = vertices[actors_parent[vertex]]
    U.newEdge(parent_id, vertex_id, style=parentChildEdge)

## Draw edges for messages exchanged
for (sender, receiver) in edges:
  width_ratio = math.log(edges[(sender, receiver)])# / float(vertex_num)
  sender_vertex = vertices[sender] 
  receiver_vertex = vertices[receiver]
  U.newEdge(sender_vertex, receiver_vertex, style=wideEdge, width=width_ratio, strength=0.05)

print "Total number of vertices: %d" % len(vertices)
print "Total number of edges: %d" % len(edges)

#pp.pprint(vertices)
