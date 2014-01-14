package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

public interface CustomEdge {
	
	public String getName();	
	public DefaultGraphCell getMIDDLEIntermediateVertex();
	public IntermediateVertex2 getLEFTIntermediateVertex();
	public IntermediateVertex3 getRIGHTIntermediateVertex();
	
	

}
