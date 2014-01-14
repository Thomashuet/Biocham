package fr.inria.contraintes.biocham.customComponents;


import fr.inria.contraintes.biocham.BiochamModel;
import javax.swing.SwingUtilities;



/**
 * Class thats holds the data of a numerical simulation result(plot).
 * The data is a reference to the model, a reference to the panel holding the image, name and filename(if it exists) and the numerical data of the simulation(PlotDataTable).
 * 
 * @author Dragana Jovanovska  
 */ 

public class PlotImage {
	
	
	private String filename=null,name=null;
	private BiochamModel model=null;
	private BiochamPlot plot=null;
	PlotDataTable tablePlotData=null;	
	private int i=0;
	
	

	public PlotImage(String n,BiochamModel m){
		
		boolean b=SwingUtilities.isEventDispatchThread();
		model=m;
		name=n;
	}
	
	public PlotImage(){}
	
	
	
	
	public int getI() {
		return i;
	}
	public void setI(int i) {
		this.i = i;
	}
	
	public String getFilename() {
		return filename;
	}
	public void setFilename(String filename) {
		this.filename = filename;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String toString(){
		return this.name;
	}
	public BiochamModel getModel() {
		return model;
	}
	public void setModel(BiochamModel m) {
		this.model = m;
	}
	public BiochamPlot getPlot() {
		return plot;
	}
	public void setPlot(BiochamPlot plot) {
		this.plot = plot;
	}
	public PlotDataTable getTablePlotData() {
		return tablePlotData;
	}
	public void setTablePlotData(PlotDataTable tablePlotData) {
		this.tablePlotData = tablePlotData;
	}
	
	public void disposePlotImage(){
		filename=null;
		name=null;
		model=null;
		if(plot!=null){
			plot.disposePlot();
		}
		plot=null;
		tablePlotData=null;
	}

}
