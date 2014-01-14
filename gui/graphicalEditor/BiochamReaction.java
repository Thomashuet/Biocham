package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import java.awt.Color;
import java.util.ArrayList;
import java.util.UUID;

public abstract class BiochamReaction extends DefaultGraphCell implements CustomEdge{

	
	/**
	 * 
	 * Abstract class that is being extended by all the reaction types: Association, Dissociation and State Transition.
	 * 
	 **/
	public BiochamReaction(Object cell){
		super(cell);
	}

	public abstract IntermediateVertex2 getLEFTIntermediateVertex();
	public abstract DefaultGraphCell getMIDDLEIntermediateVertex();	
	public abstract IntermediateVertex3 getRIGHTIntermediateVertex();
	
	public abstract void addReactant();
	public abstract void addProduct();
	public abstract void addModulator();
	public abstract void addModulator(String nm1, int stoich, boolean fromChange);
	
	public abstract String getId();
	public abstract String getName();
	
	public abstract ArrayList<UUID> getContainingMolecules();
	
	public abstract void setColor(Color blue);
}
