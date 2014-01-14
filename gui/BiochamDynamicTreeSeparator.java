package fr.inria.contraintes.biocham;


/**
 * A class that represents an empty node in the model's explorer tree just for purposes of logical context separation.
 * It separates the basic Biocham model components(reaction model editor, simulations, boolean and numerical t. properties, abstractions) 
 * from the Biocham Warnings and the Biocham Command Line. 
 * 
 * */
public class BiochamDynamicTreeSeparator {

	BiochamModel model;
	String title;
	
	public BiochamDynamicTreeSeparator(BiochamModel m,String t){
		model=m;
		title=t;
	}
	public String toString(){
		return title;
	}
	public BiochamModel getModel() {
		return model;
	}
	public void setModel(BiochamModel model) {
		this.model = model;
	}
}
