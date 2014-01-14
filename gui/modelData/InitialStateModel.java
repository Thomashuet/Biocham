package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;



/**
 * Class that holds the initial state of a biocham model, and offers its manipulation (like adding,modifying, setting state, etc.).
 * contains all of the initial state' data and the logic, or methods and functions, to manipulate the data.
 * 
 * */
public class InitialStateModel extends ModelData{

	BiochamModel biochamModel;
	LinkedHashMap<String,String> initStates,startInitialState, notDefined;//name,value!
	

	ArrayList<InitialStateView> views;
	
	
	/**
	 * A constructor that needs a handle to a biocham model object.	 * 
	 * It initializes the data lists of the class.
	 * */
	public InitialStateModel(BiochamModel m){
		biochamModel=m;	
		initStates=new LinkedHashMap<String,String>();
		notDefined=new LinkedHashMap<String,String>();
		startInitialState=new LinkedHashMap<String,String>();
		views=new ArrayList<InitialStateView>();
	}
	
	/**
	 * Adds a initial state (name, value) to the list, if its doesn't exist yet in that list. 
	 * */
	public void addInitState(String name, String value) {	
		
		initStates.put(name,value);
		if(!startInitialState.containsKey(name)){
			startInitialState.put(name,value);
		}
	}
	
	/**
	 * Deletes an initial state?
	 * 
	 * */
	public void deleteInitState(String name){		
		if(initStates.containsKey(name)){
			initStates.remove(name);
		}	
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}	
	/**
	 * Deletes all the initial states.
	 * */
	public void deleteAll(){
		initStates.clear();
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
		
	}
	
	/**
	 * Returns the biocham model these initial states belong to.
	 * 
	 **/
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	/**
	 * Sets the biocham model these initial states belong to.
	 * 
	 **/
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	/**
	 * Returns the list of initial states in the model.
	 * 
	 **/
	public LinkedHashMap<String,String> getInitStates() {
		return initStates;
	}	
	
	/**
	 * Returns a list of the existing views connected to this model.
	 * */
	public ArrayList<InitialStateView> getViews() {
		return views;
	}
	
	public LinkedHashMap<String, String> getStartInitialState() {
		return startInitialState;
	}

	public void setStartInitialState(LinkedHashMap<String, String> startInitialState) {
		this.startInitialState = startInitialState;
	}

	public LinkedHashMap<String, String> getNotDefined() {
		return notDefined;
	}

	public void setNotDefined(LinkedHashMap<String, String> notDefined) {
		this.notDefined = notDefined;
	}

	public void setInitStates(LinkedHashMap<String, String> initStates) {
		this.initStates = initStates;
	}
	
	/**
	 * Disposes the initial states model resources correctly.
	 * */
	public void disposeElement(){
		initStates.clear();	
		startInitialState.clear();
		startInitialState=null;
		initStates=null;		
		biochamModel=null;
		views.clear();
		views=null;
	}
}
