package fr.inria.contraintes.biocham.modelData;

import java.util.ArrayList;
import fr.inria.contraintes.biocham.BiochamModel;



public class SimulationModel {
	
	BiochamModel biochamModel;	
	ArrayList<SimulationView> views;
	
	public SimulationModel(BiochamModel m){
		biochamModel=m;
		views=new ArrayList<SimulationView>();
	}
	
	
	
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	public ArrayList<SimulationView> getViews() {
		return views;
	}
	public void setViews(ArrayList<SimulationView> views) {
		this.views = views;
	}
}
