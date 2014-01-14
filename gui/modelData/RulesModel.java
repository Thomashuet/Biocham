package fr.inria.contraintes.biocham.modelData;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;

import fr.inria.contraintes.biocham.BiochamModel;


/**
 * Class that holds the reaction rules of a biocham model, and offers its manipulation (like adding, searching, removing, etc.).
 * contains all of the rules' data and the logic, or methods and functions, to manipulate the data
 * */
public class RulesModel extends ModelData{

	
	BiochamModel biochamModel;
	LinkedHashMap<String,ArrayList<String>> rules;
	ArrayList<String> uknownParams;//NOT WORKED OUT YET!
	ArrayList<RulesView> views;
	
	


	/**
	 * A constructor that needs a handle to a biocham model object.	 * 
	 * It initializes the data lists of the class.
	 * */
	public RulesModel(BiochamModel m){
		biochamModel=m;	
		rules=new LinkedHashMap<String,ArrayList<String>>();
		uknownParams=new ArrayList<String>();
		views=new ArrayList<RulesView>();
	}
	
			
	/**
	 * Creates a Rule object and adds it to the list, if its doesn't exist yet in that list.
	 * A Rule object is unique if there doesn't exist any in the list that has the same parent rule and the same extended rule.
	 * Input argument is an ArrayList of 2 components/ The first one is  the parent rule, and the second is (one of) its extended one.
	 * 
	 * */
	public void addRule(String parent, String child) {	
		
		if(rules.containsKey(parent)){
			rules.get(parent).add(child);
		}else{
			rules.put(parent, new ArrayList<String>());
			rules.get(parent).add(child);
		}	
		
	}
	
	
	/**
	 * Deletes a rule(a parent rule with all its children, or just an extended one if its a reversible rule)
	 * 
	 * */
	public void deleteRule(String rule){
		// if its a parent rule to delete with all its children
		for (String key : rules.keySet()) {
		   if(key.contains(rule.trim()) || key.equals(rule.trim()) || key==rule){
			   rules.remove(key);
			   break;
		   }else{
			   for(int i=0;i<rules.get(key).size();i++){
				   if(rules.get(key).get(i).contains(rule)){
					   rules.get(key).remove(i);
					   break;
				   }
			   }			   
			}
		}

		/*if(rules.containsKey(rule)){
			rules.remove(rule);
		}*/
		// else if its only an extended rule to delete...
		/*else{
			for(String key: rules.keySet()){
				if(rules.get(key).contains(rule)){
					rules.get(key).remove(rule);
				}
			}
		}*/
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}
	
	/**
	 * Deletes all the rules.
	 * */
	public void deleteAll(){
		rules.clear();
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
	}
	
	/**
	 * Return all the extended rules of the given parent reaction rule.
	 * */
	public ArrayList<String> getAllExpandedRules(String parent){
		return rules.get(parent);
	}
	/**
	 * Returns the biocham model these rules belong to.
	 * 
	 **/
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	/**
	 * Sets the biocham model these rules belong to.
	 * 
	 **/
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	/**
	 * Returns the list of rules in the model.
	 * 
	 **/
	public Map<String,ArrayList<String>> getRules() {
		return rules;
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
	 * Returns a list of the existing views connected to this model.
	 * */
	public ArrayList<RulesView> getViews() {
		return views;
	}
	
	/**
	 * Disposes the rules model resources correctly.
	 * */
	public void disposeElement(){
		rules.clear();	
		rules=null;
		uknownParams.clear();
		uknownParams=null;
		biochamModel=null;
		views.clear();
		views=null;
	}
	
	
}
