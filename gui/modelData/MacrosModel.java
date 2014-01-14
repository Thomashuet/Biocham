package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;


/**
 * Class that holds the Macros of a biocham model, and offers its manipulation (like adding,removing, etc.).
 * contains all of the Macros' data and the logic, or methods and functions, to manipulate the data.
 * 
 * */
public class MacrosModel extends ModelData{
	
	BiochamModel biochamModel;
	LinkedHashMap<String,String> macros;	
	ArrayList<MacrosView> views;
	
	/**
	 * A constructor that needs a handle to a biocham model object.	 * 
	 * It initializes the data lists of the class.
	 * */
	public MacrosModel(BiochamModel m){
		biochamModel=m;	
		macros=new LinkedHashMap<String,String>();//name,value!
		views=new ArrayList<MacrosView>();
	}
	
	/**
	 * Adds a macro (name, value) to the list, if its doesn't exist yet in that list. 
	 * */
	public void addMacro(String name, String value) {	
		
		macros.put(name,value);	
	}	
	
	/**
	 * Deletes a Macro.
	 * 
	 * */
	public void deleteMacro(String name){
		// if its a parent rule to delete with all its children
		if(macros.containsKey(name)){
			macros.remove(name);
		}	
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}
	
	/**
	 * Deletes all the Macros.
	 * */
	public void deleteAll(){
		macros.clear();
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}
	
	/**
	 * Returns the biocham model these Macros belong to.
	 * 
	 **/
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	/**
	 * Sets the biocham model these Macros belong to.
	 * 
	 **/
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	/**
	 * Returns the list of Macros in the model.
	 * 
	 **/
	public LinkedHashMap<String,String> getMacros() {
		return macros;
	}	
	
	/**
	 * Returns a list of the existing views connected to this model.
	 * */
	public ArrayList<MacrosView> getViews() {
		return views;
	}
	
	
	/**
	 * Disposes the Macros model resources correctly.
	 * */
	public void disposeElement(){
		macros.clear();	
		macros=null;		
		biochamModel=null;
		views.clear();
		views=null;
	}
}
