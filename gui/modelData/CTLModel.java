/**
 * 
 */
package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamModel;
import java.util.ArrayList;



/**
 * Class that holds the CTL Properties of a biocham model, and offers its manipulation (like adding,removing, etc.).
 * contains all of the CTL Properties' data and the logic, or methods and functions, to manipulate the data.
 * 
 * @author Dragana Jovanovska
 * 
 * */

public class CTLModel extends ModelData{
	
	BiochamModel biochamModel;
	ArrayList<String> specifications;	
	ArrayList<CTLView> views;
	String manipulationResults;
	boolean addingGenSpec=false;
	
	public boolean isAddingGenSpec() {
		return addingGenSpec;
	}

	public void setAddingGenSpec(boolean addingGenSpec) {
		this.addingGenSpec = addingGenSpec;
	}

	/**
	 * A constructor that needs a handle to a biocham model object.	 * 
	 * It initializes the data lists of the class.
	 * */
	public CTLModel(BiochamModel m){
		biochamModel=m;	
		specifications=new ArrayList<String>();//name,value!		
		views=new ArrayList<CTLView>();
	}
	
	/**
	 * Adds a CTL Property (value) to the list, if its doesn't exist yet in that list. 
	 * */
	public void addProperty(String name) {	
		
		if(!specifications.contains(name)){
			specifications.add(name);
		}				
	}	
	
	/**
	 * Deletes a CTL Property.
	 * 
	 * */
	public void deleteProperty(String name){	
		if(specifications.contains(name)){
			specifications.remove(name);
		}	
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
		
		
	}
	
	/**
	 * Deletes all the specifications.
	 * */
	public void deleteAll(){
		specifications.clear();
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}		
	}
	
	/**
	 * Returns the biocham model these specifications belong to.
	 * 
	 **/
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	/**
	 * Sets the biocham model these specifications belong to.
	 * 
	 **/
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	/**
	 * Returns the list of specifications in the model.
	 * 
	 **/
	public ArrayList<String> getSpecifications() {
		return specifications;
	}	
	
	/**
	 * Returns a list of the existing views connected to this model.
	 * */
	public ArrayList<CTLView> getViews() {
		return views;
	}
	/**
	 * Returns the generated output from the working with the specifications.
	 * */
	public String getManipulationResults() {
		return manipulationResults;
	}
	/**
	 * Sets the generated output from the working with the specifications.
	 * */
	public void setManipulationResults(String manipulationResults) {
		this.manipulationResults = manipulationResults;
		for(int i=0;i<views.size();i++){
			views.get(i).getTarea().setText(this.manipulationResults);
		}
	}

	
	/**
	 * Disposes the specifications model resources correctly.
	 * */
	public void disposeElement(){
		specifications.clear();	
		specifications=null;		
		biochamModel=null;
		views.clear();
		views=null;
		manipulationResults=null;
		
		
	}
}

