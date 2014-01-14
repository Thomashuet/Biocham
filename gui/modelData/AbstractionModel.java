package fr.inria.contraintes.biocham.modelData;

import java.util.ArrayList;

import fr.inria.contraintes.biocham.BiochamModel;

public class AbstractionModel {

	BiochamModel biochamModel;	
	ArrayList<AbstractionView> views;
	
	

	public AbstractionModel(BiochamModel m){
		biochamModel=m;
		views=new ArrayList<AbstractionView>();
	}
	
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	public ArrayList<AbstractionView> getViews() {
		return views;
	}
	public void setViews(ArrayList<AbstractionView> views) {
		this.views = views;
	}
	
	
}
