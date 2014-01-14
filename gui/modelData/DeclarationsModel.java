package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;



/**
 * Class that holds the declarations of a biocham model, and offers its manipulation (like adding,removing, etc.).
 * contains all of the declarations' data and the logic, or methods and functions, to manipulate the data.
 * 
 * */
public class DeclarationsModel extends ModelData{

	
	BiochamModel biochamModel;
	LinkedHashMap<String,String> declarations;	
	ArrayList<DeclarationsView> views;
	
	/**
	 * A constructor that needs a handle to a biocham model object.	 * 
	 * It initializes the data lists of the class.
	 * */
	public DeclarationsModel(BiochamModel m){
		biochamModel=m;	
		declarations=new LinkedHashMap<String,String>();//name,value!	
		views=new ArrayList<DeclarationsView>();
	}
	
	/**
	 * Adds a declaration (name, value) to the list, if its doesn't exist yet in that list. 
	 * */
	public void addDeclaration(String name, String value) {	
		
		if(!declarations.containsKey(name)){			
			declarations.put(name,value);
		}
		
	}
	
	
	/**
	 * Deletes a declaration.
	 * 
	 * */
	public void deleteDeclaration(String name){	
		if(declarations.containsKey(name)){
			declarations.remove(name);
		}
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
	}
	
	/**
	 * Deletes all the declarations.
	 * */
	public void deleteAll(){
		declarations.clear();
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}	
	}
	
	/**
	 * Returns the biocham model these declarations belong to.
	 * 
	 **/
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	/**
	 * Sets the biocham model these declarations belong to.
	 * 
	 **/
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	/**
	 * Returns the list of declarations in the model.
	 * 
	 **/
	public LinkedHashMap<String,String> getDeclarations() {
		return declarations;
	}	
	
	/**
	 * Returns a list of the existing views connected to this model.
	 * */
	public ArrayList<DeclarationsView> getViews() {
		return views;
	}
		
	/**
	 * Disposes the declarations model resources correctly.
	 * */
	public void disposeElement(){
		declarations.clear();	
		declarations=null;		
		biochamModel=null;
		views.clear();
		views=null;
	}
}

