package fr.inria.contraintes.biocham.modelData;

import java.util.HashMap;
import java.util.LinkedHashMap;

import fr.inria.contraintes.biocham.BiochamModel;



public class DimensionsModel extends ModelData{

	
	
	BiochamModel biochamModel;
	LinkedHashMap<String,String> dimensions;
	
	
	/**
	 * A constructor that needs a handle to a biocham model object.	 * 
	 * It initializes the data lists of the class.
	 * */
	public DimensionsModel(BiochamModel m){
		biochamModel=m;	
		dimensions=new LinkedHashMap<String,String>();//(parameter,dimension).
	}
	
	/**
	 * Adds a dimension (parameterName, dimension) to the list, if its doesn't exist yet in that list. 
	 * */
	public void setDimension(String name, String value) {	
		
		if(!dimensions.containsKey(name)){
			dimensions.put(name,value);
		}		
	}
	
	/**
	 * Disposes the dimensions model resources correctly.
	 * */
	public void disposeElement(){
		dimensions.clear();	
		dimensions=null;		
		biochamModel=null;		
	}
	
	
}
