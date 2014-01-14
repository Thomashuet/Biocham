package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;



/**
 * Class that holds the Volumes of a biocham model, and offers its manipulation (like adding,removing, etc.).
 * contains all of the Volumes' data and the logic, or methods and functions, to manipulate the data.
 * 
 * */
public class VolumesModel extends ModelData{
	
	BiochamModel biochamModel;
	LinkedHashMap<String,String> volumes;	
	ArrayList<VolumesView> views;
	
	/**
	 * A constructor that needs a handle to a biocham model object.	 * 
	 * It initializes the data lists of the class.
	 * */
	public VolumesModel(BiochamModel m){
		biochamModel=m;	
		volumes=new LinkedHashMap<String,String>();//name,value!
		views=new ArrayList<VolumesView>();
	}
	
	/**
	 * Adds a volume (name, value) to the list, if its doesn't exist yet in that list. 
	 * */
	public void addVolume(String name, String value) {	
		
		volumes.put(name,value);		
	}	
	
	/**
	 * Deletes a volume.
	 * 
	 * */
	public void deleteVolume(String name){
		// if its a parent rule to delete with all its children
		if(volumes.containsKey(name)){
			volumes.remove(name);
		}	
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}
	
	/**
	 * Deletes all the volumes.
	 * */
	public void deleteAll(){
		volumes.clear();
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}
	
	/**
	 * Returns the biocham model these volumes belong to.
	 * 
	 **/
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	/**
	 * Sets the biocham model these volumes belong to.
	 * 
	 **/
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	/**
	 * Returns the list of volumes in the model.
	 * 
	 **/
	public LinkedHashMap<String,String> getVolumes() {
		return volumes;
	}	
	
	/**
	 * Returns a list of the existing views connected to this model.
	 * */
	public ArrayList<VolumesView> getViews() {
		return views;
	}
	
	
	/**
	 * Disposes the Volumes model resources correctly.
	 * */
	public void disposeElement(){
		volumes.clear();	
		volumes=null;		
		biochamModel=null;
		views.clear();
		views=null;
	}
}
