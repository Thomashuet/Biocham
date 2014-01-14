package fr.inria.contraintes.biocham.graphicalEditor;

import java.io.Serializable;
import java.util.Vector;

public class Reactions implements Serializable{

	protected Vector<Reaction> reaction;
	
	public Reactions(){
		 reaction = new Vector<Reaction>();
	}
	public Reactions(int size){
		 reaction = new Vector<Reaction>(size);
	}
	public Reaction[] getReactionsArray() {
		Reaction[] testBean2Array = new Reaction[reaction.size()];
		reaction.copyInto(testBean2Array);
		return testBean2Array;
	}
	
	public void setReactionsArray(Reaction[] testBean2Array) {
		reaction = new Vector<Reaction>();
		for (int i = 0; i < testBean2Array.length; i++) {
			reaction.add(testBean2Array[i]);
		}
	}
	
}
