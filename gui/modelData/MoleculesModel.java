package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamModel;

import java.util.ArrayList;


/**
 * Class that holds the parameters of a biocham model, and offers its manipulation (like adding,removing, etc.).
 * contains all of the parameters' data and the logic, or methods and functions, to manipulate the data.
 * 
 * */
public class MoleculesModel extends ModelData{
	
	BiochamModel biochamModel;
	ArrayList<String> molecules;	
	ArrayList<MoleculesView> views;
	
	/**
	 * A constructor that needs a handle to a biocham model object.	 * 
	 * It initializes the data lists of the class.
	 * */
	public MoleculesModel(BiochamModel m){
		biochamModel=m;	
		molecules=new ArrayList<String>();//name,value!		
		views=new ArrayList<MoleculesView>();
	}
	
	/**
	 * Adds a Molecule (name) to the list, if its doesn't exist yet in that list. 
	 * */
	public void addMolecule(String name) {	
		
		if(!molecules.contains(name)){
			molecules.add(name);
		}
		
	}
	
	
	/**
	 * Deletes a Molecule.
	 * 
	 * */
	public void deleteMolecule(String name){
		// if its a parent rule to delete with all its children
		if(molecules.contains(name)){
			molecules.remove(name);
		}	
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}
	
	/**
	 * Deletes all the Molecules.
	 * */
	public void deleteAll(){
		molecules.clear();
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}
	
	/**
	 * Returns the biocham model these Molecules belong to.
	 * 
	 **/
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	/**
	 * Sets the biocham model these Molecules belong to.
	 * 
	 **/
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	/**
	 * Returns the list of Molecules in the model.
	 * 
	 **/
	public ArrayList<String> getMolecules() {
		return molecules;
	}	
	
	/**
	 * Returns a list of the existing views connected to this model.
	 * */
	public ArrayList<MoleculesView> getViews() {
		return views;
	}
	
	
	/**
	 * Disposes the Molecules model resources correctly.
	 * */
	public void disposeElement(){
		molecules.clear();	
		molecules=null;		
		biochamModel=null;
		views.clear();
		views=null;
	}
}
