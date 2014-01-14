package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamModel;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;



/**
 * Class that holds the parameters of a biocham model, and offers its manipulation (like adding,removing, etc.).
 * contains all of the parameters' data and the logic, or methods and functions, to manipulate the data.
 * 
 * */
public class ParametersModel extends ModelData{

	BiochamModel biochamModel;
	LinkedHashMap<String,String> parameters;
	ArrayList<String> uknownParams;//NOT WORKED OUT YET!
	ArrayList<ParametersView> views;
	ArrayList<String> parametersNames;
	/**
	 * A constructor that needs a handle to a biocham model object.	 * 
	 * It initializes the data lists of the class.
	 * */
	public ParametersModel(BiochamModel m){
		biochamModel=m;	
		parameters=new LinkedHashMap<String,String>();//name,value!
		uknownParams=new ArrayList<String>();
		views=new ArrayList<ParametersView>();
		parametersNames=new ArrayList<String>();
	}
	
	/**
	 * Adds a parameter (name, value) to the list, if its doesn't exist yet in that list. 
	 * */
	public void addParameter(String name, String value) {	
		
		parameters.put(name,value);
		
	}
	
	
	/**
	 * Deletes a parameter.
	 * 
	 * */
	public void deleteParameter(String name){
		// if its a parent rule to delete with all its children
		if(parameters.containsKey(name)){
			parameters.remove(name);
		}	
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}
	
	/**
	 * Deletes all the parameters.
	 * */
	public void deleteAll(){
		parameters.clear();
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}
	
	/**
	 * Returns the biocham model these parameters belong to.
	 * 
	 **/
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	/**
	 * Sets the biocham model these parameters belong to.
	 * 
	 **/
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	/**
	 * Returns the list of parameters(name,value) in the model.
	 * 
	 **/
	public ArrayList<String> getParametersNames() {
		parametersNames.clear();
		Iterator it=parameters.keySet().iterator();
		while(it.hasNext()){
			parametersNames.add(it.next().toString());
		}
		it=null;		
		return parametersNames;
	}
	/**
	 * Returns the list of parameters in the model.
	 * 
	 **/
	public LinkedHashMap<String,String> getParameters() {
		return parameters;
	}
	
	/**
	 * Returns a list of the existing views connected to this model.
	 * */
	public ArrayList<ParametersView> getViews() {
		return views;
	}
	
	/**
	 * Returns the list of undefined parameters used in the rules. 
	 **/
	public ArrayList<String> getUknownParams() {
		return uknownParams;
	}
	
	/**
	 * Sets the list of undefined parameters used in the rules. 
	 * 
	 **/
	public void setUknownParams(ArrayList<String> uknownParams) {
		this.uknownParams = uknownParams;
	}
	/**
	 * Disposes the parameters model resources correctly.
	 * */
	public void disposeElement(){
		parameters.clear();	
		parameters=null;		
		biochamModel=null;
		views.clear();
		views=null;
	}
}
