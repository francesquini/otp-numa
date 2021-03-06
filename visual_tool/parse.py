#!/usr/bin/env python

import pprint
import re
import xmlrpclib
import sys
import math
import ubigraph

U = ubigraph.Ubigraph()
U.clear()

redSphere = U.newVertexStyle(shape="sphere", color="#ff0000")
wideEdge = U.newEdgeStyle(color="#ffffff", stroke="solid")#, spline=True)
parentChildEdge = U.newEdgeStyle(color="#0099ff", arrow=True,
    arrow_position=1.0, stroke="dashed")


pp = pprint.PrettyPrinter(indent=4)
ubivertices = {}
vertices = {}
edges = {}
actors_birth = {}
actors_lifespan = {}
actors_parent = {}
vertex_num = 0


def drawVertexIfNeeded(vertexName):
  if vertexName in ubivertices:
    return ubivertices[vertexName]
  else:
    if vertexName in actors_lifespan:
      ls = actors_lifespan[vertexName] / total_exec_time
    else:
      ls = 1
    vertex = U.newVertex(style=redSphere, label=vertexName, size=ls)
    ubivertices[vertexName] = vertex
    return vertex


if len(sys.argv) == 1:
  print "Please specify the name of SystemTap generated file from\
  ./traces/ folder"
  exit(0)

f = open("traces/"+sys.argv[1], "r")
all_lines = f.readlines()

start_exec_time = float(all_lines[0].split()[-1])
end_exec_time = float(all_lines[-1].split()[-1])
total_exec_time = end_exec_time - start_exec_time
print 'Total Execution Time:' +  str(total_exec_time/1000000000)

for line in all_lines:
  tokens = line.split()
  if tokens[0] == "p": #spawn
    vertex_num += 1
    if (vertex_num > 25 or vertex_num == 3):
      actors_birth[tokens[1]] = float(tokens[-1])
      vertices[tokens[1]] = 0
      actors_parent[tokens[1]] = tokens[2]
  elif tokens[0] == "s": #send
    if tokens[1] in vertices:
      if tokens[2] in vertices:
        sender = tokens[1]
        receiver = tokens[2]
	if tokens[3] == "0":
	  msg_size = 1
	else:
          msg_size = float(int(tokens[3]))
        if (sender, receiver) in edges:
          edges[(sender, receiver)] += msg_size
        else:
          edges[(sender, receiver)] = msg_size
  elif tokens[0] == "e": #exit
    if tokens[1] in vertices:
      actors_lifespan[tokens[1]] = float(tokens[-1]) - actors_birth[tokens[1]]


## Draw vertices
#for vertex in vertices.keys():
#  if vertex not in actors_lifespan.keys():
#    print "hey, what about me: %s?" % vertex
#    actors_lifespan[vertex] = parent_exit_time - actors_birth[vertex]
#  if vertex in actors_lifespan:
#    ls = actors_lifespan[vertex] / total_exec_time
#  else:
#    ls = 1
#  vertex_size = ls
#  vertices[vertex] = U.newVertex(style=redSphere, label=vertex, size=vertex_size)

## Draw edges for parent/child relationship
#for vertex in actors_parent.keys():
#  if actors_parent[vertex] in vertices.keys():
#    vertex_id =  vertices[vertex]
#    parent_id = vertices[actors_parent[vertex]]
#    U.newEdge(parent_id, vertex_id, style=parentChildEdge)

## Draw edges for messages exchanged
for (sender, receiver) in edges:
  if edges[(sender, receiver)] > 100:
    sth = math.log(edges[(sender, receiver)])
    sender_vertex = drawVertexIfNeeded(sender)
    receiver_vertex = drawVertexIfNeeded(receiver)
    U.newEdge(sender_vertex, receiver_vertex, style=wideEdge, width=1, strength=sth)

print "Total number of vertices: %d" % len(vertices)
print "Total number of edges: %d" % len(edges)

