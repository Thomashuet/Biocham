/**
 * 
 */
package fr.inria.contraintes.biocham.modelData;

import java.util.ArrayList;
import java.util.HashMap;

import fr.inria.contraintes.biocham.BiochamModel;

/**
 * @author Dragana Jovanovska
 *
 */
public class LTLModel extends ModelData{

	BiochamModel biochamModel;
	ArrayList<String> specifications;	
	ArrayList<LTLView> views;
	String manipulationResults;
	HashMap<String,String> foundParameters;
	boolean found=false, stopPrinting=false, fromBeginning=true, cmdFinished=false;
	/**
	 * A constructor that needs a handle to a biocham model object.	 * 
	 * It initializes the data lists of the class.
	 * */
	public LTLModel(BiochamModel m){
		biochamModel=m;	
		specifications=new ArrayList<String>();//name,value!		
		views=new ArrayList<LTLView>();
		foundParameters=new HashMap<String,String>();
	}
	
	/**
	 * Adds a LTL Property (value) to the list, if its doesn't exist yet in that list. 
	 * */
	public void addProperty(String name) {	
		
		if(!specifications.contains(name)){
			specifications.add(name);
		}				
	}	
	
	/**
	 * Deletes a LTL Property.
	 * 
	 * */
	public void deleteProperty(String name){
		// if its a parent rule to delete with all its children
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
	public ArrayList<LTLView> getViews() {
		return views;
	}
	/**
	 * Returns a the generated output from the working with the specifications.
	 * */
	public String getManipulationResults() {
		return manipulationResults;
	}
	/**
	 * Sets a the generated output from the working with the specifications.
	 * */
	public void setManipulationResults(String manipulationResults) {
		this.manipulationResults = manipulationResults;
		for(int i=0;i<views.size();i++){
			views.get(i).getTarea().setText(this.manipulationResults);
		}
	}

	
	public HashMap<String, String> getFoundParameters() {
		return foundParameters;
	}

	public void setFoundParameters(HashMap<String, String> foundParameters) {
		this.foundParameters = foundParameters;
	}

	public boolean isFound() {
		return found;
	}

	public void setFound(boolean found) {
		this.found = found;
	}

	public boolean isStopPrinting() {
		return stopPrinting;
	}

	public void setStopPrinting(boolean stopPrinting) {
		this.stopPrinting = stopPrinting;
	}

	public boolean isFromBeginning() {
		return fromBeginning;
	}

	public void setFromBeginning(boolean fromBeginning) {
		this.fromBeginning = fromBeginning;
	}

	public boolean isCmdFinished() {
		return cmdFinished;
	}

	public void setCmdFinished(boolean cmdFinished) {
		this.cmdFinished = cmdFinished;
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
