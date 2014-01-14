package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamModel;

import java.util.ArrayList;


/**
 * Class that holds the Invariants of a biocham model, and offers its manipulation (like adding,removing, etc.).
 * contains all of the Invariants' data and the logic, or methods and functions, to manipulate the data.
 * 
 * */
public class InvariantsModel extends ModelData{
	
	BiochamModel biochamModel;
	ArrayList<String> invariants;	
	ArrayList<InvariantsView> views;
	
	/**
	 * A constructor that needs a handle to a biocham model object.	 * 
	 * It initializes the data lists of the class.
	 * */
	public InvariantsModel(BiochamModel m){
		biochamModel=m;	
		invariants=new ArrayList<String>();//name,value!		
		views=new ArrayList<InvariantsView>();
	}
	
	/**
	 * Adds a Invariant (name, value) to the list, if its doesn't exist yet in that list. 
	 * */
	public void addInvariant(String name) {	
		
		if(!invariants.contains(name)){
			invariants.add(name);
		}		
	}	
	
	/**
	 * Deletes a Invariant.
	 * 
	 * */
	public void deleteInvariant(String name){
		// if its a parent rule to delete with all its children
		if(invariants.contains(name)){
			invariants.remove(name);
		}	
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}
	
	/**
	 * Deletes all the Invariants.
	 * */
	public void deleteAll(){
		invariants.clear();
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}
	
	/**
	 * Returns the biocham model these Invariants belong to.
	 * 
	 **/
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	/**
	 * Sets the biocham model these Invariants belong to.
	 * 
	 **/
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	/**
	 * Returns the list of Invariants in the model.
	 * 
	 **/
	public ArrayList<String> getInvariants() {
		return invariants;
	}	
	
	/**
	 * Returns a list of the existing views connected to this model.
	 * */
	public ArrayList<InvariantsView> getViews() {
		return views;
	}
	
	
	/**
	 * Disposes the Invariants model resources correctly.
	 * */
	public void disposeElement(){
		invariants.clear();	
		invariants=null;		
		biochamModel=null;
		views.clear();
		views=null;
	}
}
