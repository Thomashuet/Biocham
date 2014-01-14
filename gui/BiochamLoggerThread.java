package fr.inria.contraintes.biocham;

import fr.inria.contraintes.biocham.customComponents.BiochamPlot;
import fr.inria.contraintes.biocham.customComponents.BooleanPlot;
import fr.inria.contraintes.biocham.customComponents.CustomZoomableImagePanel;
import fr.inria.contraintes.biocham.customComponents.ImageContainer;
import fr.inria.contraintes.biocham.customComponents.PlotDataTable;
import fr.inria.contraintes.biocham.customComponents.PlotImage;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.graphicalEditor.Parser_SBGNRule2BiochamRule;
import fr.inria.contraintes.biocham.graphicalEditor.ReactionDialog;
import fr.inria.contraintes.biocham.modelData.AbstractionView;
import fr.inria.contraintes.biocham.modelData.ParamTableCTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableConservationLaws;
import fr.inria.contraintes.biocham.modelData.ParamTableDeclarations;
import fr.inria.contraintes.biocham.modelData.ParamTableEvents;
import fr.inria.contraintes.biocham.modelData.ParamTableInitConc;
import fr.inria.contraintes.biocham.modelData.ParamTableLTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableMacros;
import fr.inria.contraintes.biocham.modelData.ParamTableMolecules;
import fr.inria.contraintes.biocham.modelData.ParamTableParameters;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.modelData.ParamTableVolumes;
import fr.inria.contraintes.biocham.modelData.Parameters;
import fr.inria.contraintes.biocham.modelData.SimulationView;
import fr.inria.contraintes.biocham.modelData.ParamTableRules.Rule;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.EOFException;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.io.InterruptedIOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.imageio.ImageIO;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTree;



/**
 * Class that manages the communication between the Graphical Interface and Biocham.
 * It's a thread that waits for the answer of Biocham on the BiochamOutput stream. 
 * When there is an answer from Biocham, this class parses the answer and converts it to an action for the graphical interface.  
 * There is each communication thread for each model.
 * 
 * @author Dragana Jovanovska  
 */ 

public class BiochamLoggerThread extends Thread{


	
	
	
	public static final double DEFAULT_ZOOM_PERCENTAGE = 10.00;  
	public static int  i=0;
	
	
	public PrintStream biochamInput;     //input for the BioCham process...
	public BufferedReader biochamOutput; //response of the Biocham process...	
	
	
	Parameters param, 
			   rules,
			   initVal,
			   eventValues,
			   ctlSpecifications,
			   ltlSpecifications,
			   commands,
			   macros,
			   declarations,
			   molecules,
			   volumes,
			   conservationLaws;	
	BiochamPlot bPlot;
	private BiochamModel model;
	private String tempString="",repeatition="",ctlTempString="";
	private boolean eof = false;
	int cnt=0;
	boolean dontAdd=false;
	HashMap<String,String> plotData;
	ArrayList<String> arguments;
	String qfltlQuery="";
	private boolean conservationLawsMacros=false;
	ArrayList<String> forbiddenMacros;
	private boolean cmaesPlot=false;
	private boolean traceAnalyze=false;
	String lineBefore,lineBBefore;
	String msgBefore="";
	String inputFromGUI;
	
	// Constructor
		
	/**
	 *  The constructor gets the references of the model and its all elements.
	 *  It also creates an empty set where it will store the model's plot's data.
	 */
	public BiochamLoggerThread(BiochamModel modell) {
	      
		  super();
		  
			
	      model=modell;
	      param = modell.getParameters().getParamTable(); 
	      initVal = modell.getInitConditions().getParamTable(); 
	      eventValues=modell.getEvents().getParamTable();
	      rules=modell.getRules().getParamTable();
	      ctlSpecifications=modell.getCtlSpecifications().getParamTable();
	      ltlSpecifications=modell.getLtlSpecifications().getParamTable();
	      declarations=modell.getDeclarations().getParamTable();
	      macros=modell.getMacros().getParamTable();
	      conservationLaws=modell.getConservationLaws().getParamTable();
	      molecules=modell.getMolecules().getParamTable();
	      volumes=modell.getVolumes().getParamTable();
	      arguments=new ArrayList<String>();
	      plotData=new HashMap<String,String>();
	      forbiddenMacros=new ArrayList<String>();
		
	   
	}	
	
	
	/**
	 * The thread runs until the control variable eof is false.
	 * The variable eof is false until the connection with biocham environment is alive, or 
	 * until the end of the file it's not reached, or 
	 * until the end of the stream has been reached unexpectedly.
	 */
	public void run() {
		
		  
		
		
		
		eof = false;	
		
		while (!isEof()) {
	    	  
			
	         try {
	        	   
	        	 biochamOutput=model.getBiochamOutput();
	        	
	        	 String line=null;	        	 
	        	 if(biochamOutput!=null){
	        		 line=biochamOutput.readLine();	        		
	        	 }
	        	 if (line!=null) {
	            	           
	        		 
	        		 // read the tarea of the Biocham process...
	        		 // If the output comes when the workbench is in its commandline mode, 
	        		 // then prints the Biocham output to the command prompt area
	        		 Utils.debugMsg("BIOCHAM: "+line);
	        		
	        		 // model.getModelWarnings().append(model.getModelWarnings().getTarea(),Color.RED,"Error ");
    				 // model.getModelWarnings().append(model.getModelWarnings().getTarea(),Color.BLACK," in plotting. It's not possible to plot with the chosen options.\n");
	        		 
	        		 
	        		 String tLine=line;
        			 if(line.startsWith("[GUI]")){
        				 tLine=line.substring(6);
        			 }
        			 if(line.startsWith("biocham:")){
        				 model.getCommandLine().outputFromBiocham(tLine+"\n");
        			 }else{
        				 model.getCommandLine().outputFromBiocham(tLine+"\n");
        			 }	     
	              
	        		 if(isTraceAnalyze() && !line.contains("[GUI]")){
	        			 line="[GUI] traceAnalyze "+line;
	        		 }
	        		 
	        		 
	        		 if (line.startsWith("[GUI] plot ")){ // 1D plotting	        			            	   
	            	 
	        			
	        			 if(line.contains("Error")){
	        				 JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Error in plotting. It's not possible to plot with the chosen options.");
	        				 
	            	  
	        			 }else{
		            	  
	        			
		        				 if(!isCmaesPlot()){
		        					 
			        				 bPlot=new BiochamPlot(model);		        				 
			        				 //model.addPlot(bPlot);	   
			        				 model.setCurrentPlot(bPlot);
			        				 try{
			        					 bPlot.drawPlot(line.substring(11));
			        					 bPlot.getDrawingArea().getPlot().fillPlot();
				        				 PlotImage plotImg=new PlotImage(model.getModelName(),model);		        				
					 		        	 model.addPlotObject(plotImg);        					
					 		        	 plotImg.setI(BiochamLoggerThread.i);		 	
					 		      		     
					 		        	((SimulationView)model.getWhoPopupSimulation()).tabbedPane.add(model.getModelName()+"-"+model.getSimulationType()+"-"+model.getSimulationCounter(),bPlot);	
					 		        	
					 		        	((SimulationView)model.getWhoPopupSimulation()).tabbedPane.setSelectedIndex(((SimulationView)model.getWhoPopupSimulation()).tabbedPane.getTabCount()-1);		        				
					 		        	/*if(model.whoPopupSimulation==null){
					 		        		BiochamDynamicTree.workbench.setSimulationsPanel(model);	
					 		        	}*/
					 		        	
					 		        	 
				        				 model.getOdePlots().put(model.getModelName()+"-"+model.getSimulationType()+"-"+model.getSimulationCounter(), bPlot);
				        				 bPlot.setName(model.getModelName()+"-"+model.getSimulationType()+"-"+model.getSimulationCounter());
				        					        		
				        					 if(model.getPlotImages()!=null){
				        						 int num1=model.getPlotImages().size();
				        						 if(num1>0){
					        					 if(model.getPlotImages().get(num1-1)!=null){
					        						 model.getPlotImages().get(num1-1).setPlot(bPlot);
					        						 PlotDataTable dTable = new PlotDataTable(bPlot);
					        						 model.getPlotImages().get(num1-1).setTablePlotData(dTable);
					        					 }
				        					 }
				        				 }
 									bPlot.validate();
	   	        			 		        bPlot.getDrawingArea().getPlot().fillPlot();
	        				  			bPlot.getDrawingArea().revalidate();
	        				  			bPlot.getDrawingArea().getPlot().setSize( bPlot.getDrawingArea().getSize().width-50, bPlot.getDrawingArea().getSize().height-50);
			        				 }catch(Exception e){
			        					 
			        					 model.getModelWarnings().append(Color.GREEN.darker(), "Warning: ");
			        					 model.getModelWarnings().append(Color.BLACK,"Couldn't read the incorrect plot file."+"\n");
			 		            		
			 		            		 
			 		            		 
			        					 //JOptionPane.showMessageDialog(BiochamMainFrame.frame, "GUI didn't succeed reading the plot file: "+line.substring(11),"Warning",JOptionPane.WARNING_MESSAGE);
			        				 }
			        				 
			        					 						
			 					
			 							
		            		   }else{
		            		
		            			   bPlot=new BiochamPlot(model);
		            			   model.setCurrentPlot(bPlot);
		            			   bPlot.drawPlot(line.substring(11));
		            			   bPlot.validate();
		            			   BiochamDynamicTreeActionListener.addCmaesImageNode(bPlot, model);
						   bPlot.validate();
	  	        			   bPlot.getDrawingArea().getPlot().fillPlot();
	        				   bPlot.getDrawingArea().revalidate();
	        				   bPlot.getDrawingArea().getPlot().setSize( bPlot.getDrawingArea().getSize().width-50, bPlot.getDrawingArea().getSize().height-50);
		            		 }
	        		     }
	        			 
	        			 
	        		 }else if(line.startsWith("[GUI] booleanPlot")){
	        			 String nm=line.substring(17);
	        			
	        			 BooleanPlot p=new BooleanPlot(nm,model.getModelName()+"-"+model.getModelName(),model.getSimulationType()+"-"+model.getSimulationCounter());	
	        			 p.setPlotName(model.getModelName()+"-"+model.getSimulationType()+"-"+model.getSimulationCounter());//plotName
	        			 
	        			 ((SimulationView)model.getWhoPopupSimulation()).tabbedPane.add(model.getModelName()+"-"+model.getSimulationType()+"-"+model.getSimulationCounter(),p);	
	        			 ((SimulationView)model.getWhoPopupSimulation()).tabbedPane.setSelectedIndex(((SimulationView)model.getWhoPopupSimulation()).tabbedPane.getTabCount()-1);        			
	        			/* if(model.whoPopupSimulation==null){
		 		        		BiochamDynamicTree.workbench.setSimulationsPanel(model);	
	        			 }*/	        			 
	        			 model.getBooleanPlots().put(model.getModelName()+"-"+model.getSimulationType()+"-"+model.getSimulationCounter(), p);	        			
	        			 
	        		 }else if(line.contains("errors Unknown command:")){
	        			 String message=line.substring(12);		        
	        			 JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Uknown command!");
	        			 model.getModelWarnings().append(Color.RED.darker(), "Error: ");
	        			 model.getModelWarnings().append(Color.BLACK,message.substring(0,message.lastIndexOf(":")));		            	
	        			 model.getModelWarnings().append(Color.blue, message.substring(message.lastIndexOf(":"))+"\n");
	        		 }
	        		 else if(line.startsWith("[GUI] cmaesPlotTrue")){
	        			 
	        			 	setCmaesPlot(true);
	        			 
	        			 
	        		 }else if(line.startsWith("[GUI] cmaesPlotFalse")){
	        			 
	        			 	setCmaesPlot(false);
	        			 
	        			 
	        		 }else if (line.startsWith("[GUI] plotData ")){ 
	        			 
	        			 
	        			 boolean add=true;
	        			 int ind=line.indexOf("is");
	        			 String mol=line.substring(28,ind).trim();
	        			 mol="["+mol+"]";
	        			 if(plotData.size()>0){
	        				 for(int i=0;i<plotData.size();i++){
	        					 String k=plotData.get(mol);
	        					 if(k!=null){
	        						 String put=k+" <br> "+line.substring(15);
	        						 plotData.put(mol, put);
	        						 add=false;
	        						 break;
	        					 }
	        				 }
	        			 }
	        			 if(add){
	        				 plotData.put(mol, line.substring(15));
	        			 }
	        			 
	        			 
	        		 }else if (line.startsWith("[GUI] dot ")) { // draw a network graph of the model
	        			         
	        			 final String s=line.substring(10);  	        			 
	        			 SwingWorker sw=new SwingWorker(){

	        				UniversalScrollPane m_srollPane;
	        				CustomZoomableImagePanel zoomableImage;
	        				
							@Override
							public Object construct() {
								Image dotImage=null;
			        			 try{
		                	
			        				 File tFile=new File(Utils.is_OS_WINDOWS?s.replace("/","\\"):s);
			        				 dotImage=ImageIO.read(tFile);			        				
			        			 }catch(IOException e){
			        				  Utils.debugMsg("IO exception when doing network graph..."+e.getMessage());
			        				  e.printStackTrace();
			        			 }	
			        			 
			        			 if(dotImage!=null){
			        				 
				        			 double per = DEFAULT_ZOOM_PERCENTAGE;
				        			 try{	        				 
				        				 per = Double.valueOf(10).intValue();	        				 
				        			 }catch(NumberFormatException numExp){	     
				        				 numExp.printStackTrace();
				        			 } 
				        			 
				        			 zoomableImage=new CustomZoomableImagePanel(dotImage,per);	
				        			 ImageContainer m_imageContainer = new ImageContainer(zoomableImage,"ReactionGraph");
				        			 m_imageContainer.setName("ReactionGraph");    
				                	 m_srollPane = new UniversalScrollPane(m_imageContainer);
				                	 zoomableImage.setM_srollPane(m_srollPane);
				                	 m_srollPane.setAutoscrolls(true);	                  
				                	 MediaTracker mt = new MediaTracker(zoomableImage);
				                	 mt.addImage(dotImage, 1);
				                	 try{	                	  
				                		 mt.waitForAll(); 
				                	 }catch (InterruptedException e){ 
				                		 e.printStackTrace(); 
				                	 
				                	 }				                	 
				                	 zoomableImage.repaint();      
				                	 mt=null;
			        			 }else{
			        				 JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Error accessing file system for viewing the reaction Graph.");
			        			 }
								return null;
							}

							@Override
							public void finished() {
								model.setRgCounter(model.getRgCounter()+1);
								model.getModelEditor().tabbedPane.add(model.getModelName()+"-ReactionGraph"+"-"+model.getRgCounter(),m_srollPane);	
								model.getModelEditor().getTabbedPane().setSelectedIndex(model.getModelEditor().getTabbedPane().getTabCount()-1);		        				
								BiochamDynamicTree.workbench.setModelEditor(model);
			 		        	 
								//BiochamDynamicTreeActionListener.getCorrespondigTabbedPane().add(model.getModelName()+"-"+"Reaction Graph", m_srollPane);	
								//BiochamDynamicTreeActionListener.getCorrespondigTabbedPane().setSelectedIndex(BiochamDynamicTreeActionListener.getCorrespondigTabbedPane().getTabCount()-1);								
			                	model.setReactionGraph(m_srollPane);           
			                	model.setRGraph(zoomableImage);			                	
								
							}};
	        			 sw.start();
	        			 
	        			             	 
	                	
	                	 
	                	 
	        		 }else if(line.startsWith("[GUI] dotNetwork ")){ //draw a network graph of the model
	            	
		                  final String s=line.substring(17);		             
		                  SwingWorker sw=new SwingWorker(){

		                	CustomZoomableImagePanel zoomableImage=null;
		                	UniversalScrollPane m_srollPane=null;
							@Override
							public Object construct() {
								 Image dotImage=null,zoom=null;
				                  double per = DEFAULT_ZOOM_PERCENTAGE;
				                  
				                  try{
				                					                				        			  	 
				                	  File tFile=new File(Utils.is_OS_WINDOWS?s.replace("/","\\"):s);
				                	  dotImage=ImageIO.read(tFile);				                	
				                	  
				                  }catch(IOException e){
				                	  Utils.debugMsg("IO exception when doing network graph..."+e.getMessage());
				                	  e.printStackTrace();				                	
				                  }
				                  	
				                  if(dotImage!=null){
					                  try{		                  
					                      per = Double.valueOf(10).intValue();
					                  }catch(NumberFormatException numExp){		
					                	  numExp.printStackTrace();
					                  }        
					                  
					                  zoomableImage=new CustomZoomableImagePanel(dotImage,per);	 
					                  ImageContainer m_imageContainer = new ImageContainer(zoomableImage);
					                  m_srollPane = new UniversalScrollPane(m_imageContainer);
					                  zoomableImage.setM_srollPane(m_srollPane);
					                  m_srollPane.setAutoscrolls(true);		                  
					                  MediaTracker mt = new MediaTracker(zoomableImage);
					                  mt.addImage(dotImage, 1);
					                  mt.addImage(zoom, 2);				                  
				                	 
					                  try{		                   
					                      mt.waitForAll(); 
					                  }catch (InterruptedException e){		                  
					                      e.printStackTrace(); 
					                  }
					                  
					                  zoomableImage.repaint();
				                  }else{
				                	  JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Error accessing file system for viewing the network Graph.");
				                  }
								return null;
							}

							@Override
							public void finished() {
								
								if(getInputFromGUI().contains("neighborhood")){
									model.setIgCounter(model.getNhCounter()+1);
									((AbstractionView)model.getWhoPopup()).tabbedPane.add(model.getModelName()+"-NeighborhoodGraph"+"-"+model.getNhCounter(),m_srollPane);
								}else{//influences
									model.setIgCounter(model.getIgCounter()+1);
									((AbstractionView)model.getWhoPopup()).tabbedPane.add(model.getModelName()+"-InfluencesGraph"+"-"+model.getIgCounter(),m_srollPane);
									model.setInfluencesGraph(m_srollPane);
									model.setIGraph(zoomableImage);
								}									
								((AbstractionView)model.getWhoPopup()).getTabbedPane().setSelectedIndex(((AbstractionView)model.getWhoPopup()).getTabbedPane().getTabCount()-1);		        				
								BiochamDynamicTree.workbench.setAbstractionsEditor(model);
				               
							}};
		                  sw.start();
		                  
		                 
		                  
	        		 
		              
		                  
		                  
	        		 } else if (line.startsWith("[GUI] param ")) { // set the parameters
	        		
	        			 
	        			 int i = line.substring(12).indexOf(',');
	        			 String p = line.substring(12,12+i);
	        			 int j = line.length();
	        			 if (line.charAt(j-1) == '.')
	        				 j--;
	        			 /*float v = 0;
	        			 try{
	        				 v=Float.parseFloat(line.substring(12+i+1,j));
	        			 }catch(Exception e){
	        				 System.out.println("Parameter "+p+" has incorrect value...."+v+"?");
	        				 WorkbenchToolBars.infoLabel.setText("Parameter "+p+" has incorrect value...."+v+"? Value set to default(0).");
		            		 WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);
	        			 }	        */			 	
	        			 if(!((ParamTableParameters)param).isNotToAdd()){
	        				 /*arguments.clear();
	        				 arguments.add(p);
	        				 arguments.add(new Float(v).toString());	
	        				 param.setValue(arguments);*/
	        				 ((ParamTableParameters)param).getParametersModel().addParameter(p,line.substring(12+i+1,j));
	        				 for(int c=0;c<((ParamTableParameters)param).getParametersModel().getViews().size();c++){
			            		   ((ParamTableParameters)param).getParametersModel().getViews().get(c).refresh();
	        				 }
	        				 
	        				// arguments.clear();	                 
	        			 }else{
	        				 ((ParamTableParameters)param).setNotToAdd(false);
	        			 }	              
	        		       		 
	        		 }else if(line.startsWith("[GUI] addParam")){
	        			 String paramName=line.substring(line.indexOf("(")+1,line.indexOf(","));
	        			 String value=line.substring(line.indexOf(",")+1,line.indexOf(")"));
	        			 ((ParamTableParameters)param).getParametersModel().addParameter(paramName,value);
        				 for(int c=0;c<((ParamTableParameters)param).getParametersModel().getViews().size();c++){
		            		   ((ParamTableParameters)param).getParametersModel().getViews().get(c).refresh();
        				 }
	        			/* arguments.clear();
        				 arguments.add(paramName);
        				 arguments.add(new Float(value).toString());	
        				 param.setValue(arguments);
        				 for(int c=0;c<((ParamTableParameters)param).getParametersModel().getViews().size();c++){
		            		   ((ParamTableParameters)param).getParametersModel().getViews().get(c).refresh();
        				 }
        				 arguments.clear();	 */
	        			 
	        		 }else if(line.startsWith("[GUI] deleteParameter")){
	        			 	        			
	        			 String p = line.substring(22);	
	        			 
	        		 }else if (line.startsWith("[GUI] molecule ")) { // set the molecules
	        			 
	        			 
	        			 int k=line.substring(15).indexOf('.');	        			 
	        			 ((ParamTableMolecules)molecules).getMoleculesModel().addMolecule(line.substring(15,15+k));
        				 for(int c=0;c<((ParamTableMolecules)molecules).getMoleculesModel().getViews().size();c++){
		            		   ((ParamTableMolecules)molecules).getMoleculesModel().getViews().get(c).refresh();
        				 }
	        			 /*arguments.clear();
	        			 arguments.add(molecule);		             
	        			 molecules.setValue(arguments);
	        			 for(int c=0;c<((ParamTableMolecules)molecules).getMoleculesModel().getViews().size();c++){
		            		   ((ParamTableMolecules)molecules).getMoleculesModel().getViews().get(c).refresh();
	        			 }
	        			 arguments.clear();
	                 */
	        			 
	        			 
	        			 
	        		 }else if (line.startsWith("[GUI] add_spec ")) { // set the CTL specifications
	        			 
	        			 String ctlSpec=line.substring(15);
	        			 if(ctlSpec.contains("FINISHED")){
	        				 ((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().setAddingGenSpec(false);
        					 ((ParamTableCTLSpecifications)ctlSpecifications).setAddingGenerated(false);
        					 ((ParamTableCTLSpecifications)ctlSpecifications).checkAddingGenerated();        					 
        					 for(int c=0;c<((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().getViews().size();c++){
			            		   ((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().getViews().get(c).refresh();
	        				 }
        				 }else{        					 
        					 ((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().addProperty(ctlSpec);        					 
        					 if(!((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().isAddingGenSpec()){
        						 for(int c=0;c<((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().getViews().size();c++){
  			            		   ((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().getViews().get(c).refresh();
        						 } 
        					 }
        				 }               
	        			 
	        			 
	        		 }else if(line.startsWith("[GUI] dontAddMacro")){
	        			 String macro=line.substring(18);
	        			 if(!forbiddenMacros.contains(macro)){
	        				 forbiddenMacros.add(macro);
	        			 }
	        			 
	        		 }else if(line.startsWith("[GUI] conservationLaws")){
	            	   
	        			 
	        			 conservationLawsMacros=true;
	        			 String command=line.substring(23);	 	            	             	   	            	   
	        			 
	        			 if(command.startsWith("P-Invariants:")){
	        				
	        				 command=command.substring(14);
	        				 ParamTableConservationLaws cLaws_check=(ParamTableConservationLaws)conservationLaws;				             
	        				 JPanel pan=cLaws_check.getCheckingPanel();
	        				 Component[] comps=pan.getComponents();
	        				 int size=comps.length;	
	        				 for(int i=0;i<size;i++){
	        					 if(comps[i] instanceof JTextArea){
	        						 JTextArea ta=(JTextArea)comps[i];
	        						 ta.append(" "+command+"\n");
	        						 
	        					 }
	        				 }
	        				// pan.validate();
	        				 pan.repaint();
	        				 pan=null;
	        				 comps=null;
	        				 conservationLawsMacros=false;
	        				 
	        			 }else if(command.startsWith("Delete:")){
	            		 
	        				 command=command.substring(8);
	        				 ParamTableConservationLaws cLaws_check=(ParamTableConservationLaws)conservationLaws;				             
	        				 JPanel pan=cLaws_check.getCheckingPanel();
	        				 Component[] comps=pan.getComponents();
	        				 int size=comps.length;	
	        				 for(int i=0;i<size;i++){
	        					 if(comps[i] instanceof JTextArea){
	        						 JTextArea ta=(JTextArea)comps[i];
	        						 ta.append("\n "+command+"\n");		
	        						 
	        					 }
	        				 }				              
	        				// pan.validate();
	        				 pan.repaint();
	        				 pan=null;
	        				 comps=null;	
	        				 conservationLawsMacros=false;
	            		   
	        			 }else if(command.startsWith("Add:")){
	            	
	        				 command=command.substring(5);
	        				 ((ParamTableConservationLaws)conservationLaws).getInvariantsModel().addInvariant(command);
	        				 for(int c=0;c<((ParamTableConservationLaws)conservationLaws).getInvariantsModel().getViews().size();c++){
			            		   ((ParamTableConservationLaws)conservationLaws).getInvariantsModel().getViews().get(c).refresh();
	        				 }
	        				/* arguments.clear();
	        				 arguments.add(command);
	        				 conservationLaws.setValue(arguments);
	        				 for(int c=0;c<((ParamTableConservationLaws)conservationLaws).getInvariantsModel().getViews().size();c++){
			            		   ((ParamTableConservationLaws)conservationLaws).getInvariantsModel().getViews().get(c).refresh();
			            	   }*/
	            		   
	        			 }else if(command.startsWith("Stop")){
	        				 conservationLawsMacros=false;
	        			 }else if(command.startsWith("Check:")){
	            		   
	        				 command=command.substring(7);            		  
	        				 ParamTableConservationLaws cLaws_check=(ParamTableConservationLaws)conservationLaws;				             
	        				 JPanel pan=cLaws_check.getCheckingPanel();
	        				 Component[] comps=pan.getComponents();
	        				 int size=comps.length;	
	        				 for(int i=0;i<size;i++){
	        					 if(comps[i] instanceof JTextArea){
	        						 JTextArea ta=(JTextArea)comps[i];
	        						 if(!command.equals(repeatition)){
	        							 ta.append("\n "+command+"\n");
	        						 }	            		  
	        					 }
	        				 }	
	        				 repeatition=command;
	        				 //pan.validate();
	        				 pan.repaint();
	        				 pan=null;
	        				 comps=null;	
	        				 conservationLawsMacros=false;
	        			 }
	              
	        			 
	        			 conservationLawsMacros=false; // NEW ADDED!!!!!!!!!!!!!!!!
	        			 
	        		 
	        		 }else if (line.startsWith("[GUI] checkCTL") || line.startsWith("[GUI] CheckCTL")) { 
		            	  
	        			 String message=line.substring(14);	     			        					        					
	            		 if(message.contains("Loading") || message.contains("satisfied") ){
	            			 ((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().getViews().get(0).appendResults("\n");
						 }
	            		 ((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().getViews().get(0).appendResults(message+"\n");	
						 ctlTempString=message;	 	        						            		 
	        		
	        			
	        		 }else if(line.startsWith("[GUI] genCTL")){
	        			 
	        			 String message=line.substring(12);	    			     					
	            		 if(message.contains("START")){
	            			 ((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().getViews().get(0).appendResults("\nGenerated CTL properties:\n"); 
	            			 ((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().getViews().get(0).appendResults("\n");
	            		 }else{
	            			 ((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().getViews().get(0).appendResults(message+"\n");
	            		 }	            		 
	        			 
	        			 
	        		 }else if (line.startsWith("[GUI] add_ltl ")) {
		            	  
	        			 
	        			 ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().addProperty(line.substring(14));        					 
	        			 for(int c=0;c<((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getViews().size();c++){
		            		   ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getViews().get(c).refresh();
						 } 
	        			/* ((ParamTableLTLSpecifications)ltlSpecifications).setModifying(false);
	        			 String ltlSpec=line.substring(14);
	        			 if(!((ParamTableLTLSpecifications)ltlSpecifications).isNotToAdd()){
	        				 arguments.clear();
	        				 arguments.add(ltlSpec);		             
	        				 ltlSpecifications.setValue(arguments);
	        				 arguments.clear();
	        				
	        			 }else{
	        				 ((ParamTableLTLSpecifications)ltlSpecifications).setNotToAdd(false);
	        				 ((ParamTableLTLSpecifications)ltlSpecifications).setModifying(false);
	        			 }
	         */
	        		 
	        		 
	        		 
	        		 }else if(line.startsWith("[GUI] robustness")){
	            	   	        	
	        			 ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getViews().get(0).appendResults(line.substring(16)+"\n");			        					        					
	            		 
						 
	        			/* String message=line.substring(16);               
	        			 ParamTableLTLSpecifications ltl_check=(ParamTableLTLSpecifications)ltlSpecifications;
	        			 JPanel pan=ltl_check.getLTLModelChecking();
	        			 Component[] comps=pan.getComponents();
	        			 int size=comps.length;	
	        			 for(int i=0;i<size;i++){
	        				 if(comps[i] instanceof JTextArea){
	        					 JTextArea ta=(JTextArea)comps[i];
	        					 ta.append(message+"\n");
	        				 }
	        			 }
	        			 pan.repaint();
	        			 pan=null;
	        			 comps=null;
	        			 ltl_check=null;*/
	
	        		 
	        		 
	        		 }else if(line.startsWith("[GUI] landscape")){
	           
	        			 
	        			 String message=line.substring(15);  
	        			 if(message.contains("FINISHED")){
	        				 message="The landscape is being calculated.\n If you have correctly installed GNUPLOT, data files and landscape image is being created.";
	        			 }
	        			 ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getViews().get(0).appendResults(message+"\n");	
	        			 message=null;
	        			/* ParamTableLTLSpecifications ltl_check=(ParamTableLTLSpecifications)ltlSpecifications;
	        			 JPanel pan=ltl_check.getLTLModelChecking();
	        			 Component[] comps=pan.getComponents();
	        			 int size=comps.length;	
	        			 for(int i=0;i<size;i++){
	        				 if(comps[i] instanceof JTextArea){
	        					 JTextArea ta=(JTextArea)comps[i];
	        					 ta.append(message+"\n");
	        				 }
	        			 }	
	        			 pan.repaint();
	        			 pan=null;
	        			 comps=null;
	        			 ltl_check=null;*/
	                
	        	
	        			 
	        			 
	        		 }else if (line.startsWith("[GUI] checkLTL") || line.contains("[GUI] checkLTL ")) { 
	        			 
		            	  
	        			 if((line.contains("Searched parameter") || line.contains("Parameter")) && line.contains("should") && line.contains("initial value")){
	        				 if(model.getWho()!=null){
	        					 if(model.getWho().equals("LTL")){
	        						 ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getViews().get(0).appendResults(line.substring(14)+"\n Correct the mistake and try again.\n");	
	        						/* ((ParamTableLTLSpecifications)ltlSpecifications).getTarea().append(line.substring(14)+"\n Correct the mistake and try again.\n");
	        						 ((ParamTableLTLSpecifications)ltlSpecifications).setStopPrinting(true);*/
	        						 ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().setStopPrinting(true);
	        					 }
	        				 }
	        			 }else{
	        				 String message="";
	        				 if(!line.startsWith("[GUI] checkLTL")){
	        					 int ind=line.indexOf("[GUI] checkLTL");
	        					 String temp=line.substring(ind);
	        					 message=temp.substring(14);		            	   
	        				 }else{
	        					 message=line.substring(14);
	        				 }
	        				
	        				/* ParamTableLTLSpecifications ltl_check=(ParamTableLTLSpecifications)ltlSpecifications;
	        				 JPanel pan=ltl_check.getLTLModelChecking();
	        				 Component[] comps=pan.getComponents();
	        				 int size=comps.length;	
	        				 for(int i=0;i<size;i++){
	        					 if(comps[i] instanceof JTextArea){
	        						 JTextArea ta=(JTextArea)comps[i];*/
	        						 if(((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().isFromBeginning()){
	        							 ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().setFromBeginning(false);
	        						 }
	        						 if(!((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().isStopPrinting()){
	        							 ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getViews().get(0).appendResults(message+"\n");
	        							 if(message.contains("parameter(") && ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().isFound()){
	        								 ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getFoundParameters().put(message.substring(message.indexOf("(")+1,message.indexOf(",")),message.substring(message.indexOf(",")+1,message.indexOf(")")));
	        							 }
	        							 if(message.contains("Found")){
	        								 ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().setFound(true);
	        							 }
	        							
	        							 if(message.contains("Time elapsed") && msgBefore.contains("parameter")){
	        								 ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().setCmdFinished(true);	        								 
	        								 if(((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().isCmdFinished() && ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().isFound()){	        									 
	        									 StringBuffer buf=new StringBuffer();	        								
	        									 buf.append("Do you want to apply the found parameters?\n");
	        									 Map.Entry e=null;
	        									 for(Iterator it=((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getFoundParameters().entrySet().iterator();it.hasNext();){
	        										e=(Map.Entry)it.next();    										
	        										buf.append((String)e.getKey());
	        										buf.append(",");
	        										buf.append((String)e.getValue());
	        										buf.append("\n");	 
	        										
	        									 }
		        								 int rep=JOptionPane.showConfirmDialog(BiochamMainFrame.frame,buf.toString(),"Parameters' Values Found",JOptionPane.YES_NO_OPTION);
		        								 if(rep==JOptionPane.YES_OPTION){
		        									 buf.delete(0,buf.length());
		        									 for(Iterator it=((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getFoundParameters().entrySet().iterator();it.hasNext();){		        										 
		        										 buf.append("parameter(");
		        										 e=(Map.Entry)it.next();		        										 
		        										 buf.append((String)e.getKey());
		        										 buf.append(",");
		        										 buf.append((String)e.getValue());
		        										 buf.append(").\n");		        										 
		        									 }
		        									 ((ParamTableParameters)model.getParameters().getParamTable()).setDontAskForModify(true);
		        									 model.sendToBiocham(buf.toString());
		        									 
		        								 }
		        								 buf=null;
		        								 e=null;
		        							 }
	        							 }
	        							 
	        							 cnt++;		        					
	        							 if(cnt>5 && ! ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getManipulationResults().contains("It is not recommended")){	        						 
	        								 ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getViews().get(0).appendResults("\n It is not recommended that you start another task until this is not finished.\n If you want to stop this task, click on <Stop Execution> from the toolbar.\n\n");
	        								 cnt=0;	        						
	        							 }	 
	        						 }        					 		            		   
	        					//}
	        			//	 }		              
	        				 tempString=message;
	        				 //pan.validate();
	        				/* pan.repaint();
	        				 pan=null;
	        				 comps=null;
	        				 ltl_check=null;	*/
	        				 msgBefore=line;
	        			 }	        			 
		               
	               }else if(line.startsWith("[GUI] loadTrace FINISHED")){
	            	   
	            	   ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getViews().get(0).appendResults("\n Trace loaded."+"\n");
	            	   /*	ParamTableLTLSpecifications ltl_check=(ParamTableLTLSpecifications)ltlSpecifications;
	        			 JPanel pan=ltl_check.getLTLModelChecking();
	        			 Component[] comps=pan.getComponents();
	        			 int size=comps.length;	
	        			 for(int i=0;i<size;i++){
	        				 if(comps[i] instanceof JTextArea){
	        					 JTextArea ta=(JTextArea)comps[i];
	        					 ta.append("\n Trace loaded."+"\n");	        					             		   
	        				 }
	        			 }		              
	        			 comps=null;
	        			 pan=null;
	        			 ltl_check=null;*/
	            	   
	               }else if(line.startsWith("[GUI] traceAnalyze ")){
	            	   
	    
		               String message="";
		               message=line.substring(19);
		               
		               if(message.contains("start")){
		            	   setTraceAnalyze(true);
		               }else if(message.contains("stop")){
		            	   setTraceAnalyze(false);
		               }else if(message.contains("elapsed")){
		            	   setTraceAnalyze(false);
		               }
		               else{
		            	   
			               if(message.trim().equals("1")){
			            	   message=" "+qfltlQuery+" is TRUE on the trace.";
			            	   qfltlQuery="";
			            	   dontAdd=false;
			               }else if(message.trim().equals("0")){
			            	   message=" "+qfltlQuery+" is FALSE on the trace.";
			            	   qfltlQuery="";
			            	   dontAdd=false;
			               }else{
			            	   qfltlQuery=message;
			            	   //dontAdd=true;
			               }
			               
			               if(!dontAdd){
				              /* ParamTableLTLSpecifications ltl_check=(ParamTableLTLSpecifications)ltlSpecifications;
				               JPanel pan=ltl_check.getLTLModelChecking();
				               Component[] comps=pan.getComponents();
				               int size=comps.length;	
				               for(int i=0;i<size;i++){
				            	   if(comps[i] instanceof JTextArea){
				            		   JTextArea ta=(JTextArea)comps[i];*/
				            		   if( ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().isFromBeginning()){
				            			  // ta.append("\n\n");
				            			   ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().setFromBeginning(false);
				            		   }
				            		  if(!message.equals(tempString)){
				            			  ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getViews().get(0).appendResults("\n"+" "+message);
				            		  }else{
				            			  cnt++;		            			  
				            		  }
				            		  if(cnt>5){
				            			  ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getViews().get(0).appendResults("In progress....Please wait.....\n");
				            			  cnt=0;
				            		//  }		            		   
				            	//   }
				               }		              
				               tempString=message;
				               //pan.validate();
				              /* pan.repaint();
				               pan=null;
				               comps=null;
				               ltl_check=null;*/
			               
			               }
	               
		               }
		            
		           
		               
		               
	               }else if (line.startsWith("[GUI] macro ")) { // set the macros
	            	   
	            	   if(!conservationLawsMacros){
		            	   int i = line.substring(12).indexOf(',');
		            	   String p = line.substring(12,12+i);
		            	   if(!p.startsWith("[")){
			            	   int j = line.length();
			            	   if (line.charAt(j-1) == '.')
			            		   j--;
			            	   String v = line.substring(12+i+1,j);
			            	   if(!((ParamTableMacros)macros).isNotToAdd() &&  !((ParamTableMacros)macros).isModifying()){
			            		   ((ParamTableMacros)macros).getMacrosModel().addMacro(p, v);
			        				 for(int c=0;c<((ParamTableMacros)macros).getMacrosModel().getViews().size();c++){
					            		   ((ParamTableMacros)macros).getMacrosModel().getViews().get(c).refresh();
			        				 }
			            		   /*arguments.clear();
			            		   arguments.add(p);
			            		   arguments.add(v);		                 
			            		   macros.setValue(arguments);
			            		   for(int c=0;c<((ParamTableMacros)macros).getMacrosModel().getViews().size();c++){
				            		   ((ParamTableMacros)macros).getMacrosModel().getViews().get(c).refresh();
				            	   }
			            		   arguments.clear();*/
			            	   }else{
			            		   ((ParamTableMacros)macros).setNotToAdd(false);
			            		   ((ParamTableMacros)macros).setModifying(false);
			            	   }
		            	   }
	            	   }else{
	            		   conservationLawsMacros=false;
	            	   }

	            	   
	            	
	            	   
	            	   
	            	   
	               }else if (line.startsWith("[GUI] volume ")) { // set the volumes
	            	               	
	            	  	              
	            	   int i = line.substring(13).indexOf(',');
		               String p = line.substring(13,13+i);
		               int j = line.length();
		               if (line.charAt(j-1) == '.')
		            	   j--;
		               String v = line.substring(13+i+1,j);                	                  
		               if(!((ParamTableVolumes)volumes).isDeleting() && !((ParamTableVolumes)volumes).isNotToAdd()){
		            	   
		            	   ((ParamTableVolumes)volumes).getVolumesModel().addVolume(p, v);
		            	   for(int c=0;c<((ParamTableVolumes)volumes).getVolumesModel().getViews().size();c++){
		            		   ((ParamTableVolumes)volumes).getVolumesModel().getViews().get(c).refresh();
		            	   }
			              /* arguments.clear();
			               arguments.add(p);
			               arguments.add(v);		                 
			               volumes.setValue(arguments);
			               for(int c=0;c<((ParamTableVolumes)volumes).getVolumesModel().getViews().size();c++){
		            		   ((ParamTableVolumes)volumes).getVolumesModel().getViews().get(c).refresh();
		            	   }
			               arguments.clear();*/
			               
		               }else{
		            	   ((ParamTableVolumes)volumes).setDeleting(false);
		            	   ((ParamTableVolumes)volumes).setNotToAdd(false);
		               }		              
		                  
	               
		               
		               
		               
		               
	               }else if(line.startsWith("[GUI] file")){
	            	   
	            	   ((ParamTableRules)rules).setIgnoreUndefinedParametersWarnings(true);
	            	   BiochamDynamicTree.currentModel=model;
	            	   Utils.debugMsg("\n\n\n\n[GUI] file: Set CurrentModel="+model.getModelName()+"\n\n\n\n");
	            	   model.setLoadingModel(true);
	               }             
	               else if(line.startsWith("[GUI] clearRules")){
	            	   ((ParamTableRules)rules).getRulesModel().deleteAll();
	            	    model.sendToBiocham("list_molecules.\n");
	               }else if(line.startsWith("[GUI] reduction")){
	            	   if(line.contains("Looking for")){
	            		   ((AbstractionView)model.getWhoPopup()).appendOutput("\n\n");
	            	   }
	            	   ((AbstractionView)model.getWhoPopup()).appendOutput(line.substring(16));	  
	               }
	               else if (line.startsWith("[GUI] add_rule ")) { // set the rules
	            	   
	            	             
	            	 
	            	   conservationLawsMacros=true;
	            	   if(line.substring(15).contains("Error")){//Error: unknown parameter or molecule
	            		   ((ParamTableRules)rules).setAddRule(true);
	            		   if(!((ParamTableRules)rules).getUknownParams().contains(line.substring(53))){
	            			   ((ParamTableRules)rules).getUknownParams().add(line.substring(53));	            			   
	            			   ((ParamTableRules)rules).refreshAfterAddingNew();
	            			   Utils.debugMsg("UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU55555555555555");
	            		   }
	            		  
	            	   }else{	        
		            	   int i = line.substring(15).indexOf(';');
			               String p = line.substring(15,15+i);			               
			               int k = line.substring(15+i+1).indexOf(';');
			               String v = line.substring(15+i+1,15+i+1+k);
			               String m = line.substring(15+i+1+k+1);
			               if(!((ParamTableRules)rules).isDeleting() && !((ParamTableRules)rules).isAdding() && !((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).isRevising()){		
			            	  /* arguments.clear();
			            	   arguments.add(p);
			            	   int exists = ((ParamTableRules)rules).indexOf(p);
			            	   arguments.add(v+" for "+m);		              
			            	   rules.setValue(arguments);
			            	   arguments.clear();
			            	  
			            	   //((ParamTableRules)rules).getRmodel().addRule(p, v+" for "+m);
			            	   for(int c=0;c<((ParamTableRules)rules).getRulesModel().getViews().size();c++){
			            		   ((ParamTableRules)rules).getRulesModel().getViews().get(c).refresh();
			            	   }*/
			            	   int exists = ((ParamTableRules)rules).indexOf(p);
			            	   ((ParamTableRules)rules).getRulesModel().addRule(p,v+" for "+m);
			            	   for(int c=0;c<((ParamTableRules)rules).getRulesModel().getViews().size();c++){
			            		   ((ParamTableRules)rules).getRulesModel().getViews().get(c).refresh();
			            	   }
			            	   
			            	   if(!((ParamTableRules)rules).isDontDraw()){
			            		   if(exists<0 && Parser_SBGNRule2BiochamRule.isFromGraph()){
				            		   Utils.debugMsg("HIIIIIHIHIHIHHHIIIIIIIIIII");
					            	   Parser_SBGNRule2BiochamRule.drawRule();
					            	   Parser_SBGNRule2BiochamRule.setFromGraph(false);          	   
				            	   }else if(exists<0 && !Parser_SBGNRule2BiochamRule.isFromGraph() && !Parser_SBGNRule2BiochamRule.isDontDraw() && Parser_SBGNRule2BiochamRule.getDople()<2){
				            		   if(p.contains("?") || p.contains("$") || p.contains("where") || containsReactionShortcut(p)){
				            			   model.parserBcRule2Graph.parse(m);
				            		   }else{
				            			   model.parserBcRule2Graph.parse(p);
				            		   }
				            	   }else{
				            			Parser_SBGNRule2BiochamRule.setDontDraw(false);
				            			Parser_SBGNRule2BiochamRule.setDople(0);
				            	   }
			            	   }else{
			            		   ((ParamTableRules)rules).setCounter(((ParamTableRules)rules).getCounter()-1);
			            	   }
			            	   
			            	   
			            	 /*  if(!Parser_SBGNRule2BiochamRule.ruleValidationPassed){
			            		  
			            	   }*/
			            	 
			            	   
			            	   
			               }else{
			            	   Rule rule=((ParamTableRules)rules).createNewRule(p,v+" for "+m);
			            	   ((ParamTableRules)rules).addRule(rule);
			            	   Component[] comps=((ParamTableRules)rules).getPanel().getComponents();
			            	   ArrayList cs=new ArrayList();
			            	   for(int w=0;w<comps.length;w++){	
			            		   if((comps[w] instanceof JTree)){									
			            			   cs.add(comps[w]);
			            		   }			    			    		
			    			    }		    			    
			            	   ((ParamTableRules)rules).getPanel().removeAll();
			            	   Utils.debugMsg("UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU44444444444444");
			            	   ((ParamTableRules)rules).refreshPanel(cs);
			            	   comps=null;
			            	   cs.clear();
			            	   cs=null;
			            	   rule=null;
			               }
	            	   }
	            	   conservationLawsMacros=false; 
            	   
	            	  // model.sendToBiocham("list_ODE.\n","kinetics"); 
	            	   
	            	   
	               }else if(line.startsWith("[GUI] delete_rule")){
	            	   model.getGraphEditor().getGraph().setRuleDeleted(true);
	            	   ((ParamTableRules)rules).getRulesModel().deleteRule(line.substring(17));
	            	   Utils.debugMsg("DELETE_RULES here is it: "+line.substring(17));
	            	  // model.sendToBiocham("list_ODE.\n","kinetics");
	               }             
	               else if (line.startsWith("[GUI] declare")){// || line.startsWith("declare")) { // set the declarations
	            	   
	            	   String molecule,partsof;		               
		               int i = line.substring(14).indexOf(';');		               
		               molecule=line.substring(14,14+i);		        
		               int j = line.length();
		               if (line.charAt(j-1) == '.')
		                     j--;
		               partsof = line.substring(14+i+2,j-1);
		               
	            	   if(!((ParamTableDeclarations)declarations).isNotToAdd()){	
	            		 
	            		   ((ParamTableDeclarations)declarations).getDeclarationsModel().addDeclaration(molecule, partsof);
		            	   for(int c=0;c<((ParamTableDeclarations)declarations).getDeclarationsModel().getViews().size();c++){
		            		   ((ParamTableDeclarations)declarations).getDeclarationsModel().getViews().get(c).refresh();
		            	   }
		            	   
		               }else{	
		            		((ParamTableDeclarations)declarations).adjustModifiedValue(molecule,partsof);           	  
		               }		              
		                  
	               
	               
	               }else if (line.startsWith("[GUI] add_event")) {// set the events
	            	   	            	   
	            	   String condition,name,kinetics;		    
	            	   int i = line.substring(16).indexOf(';');
	            	   condition=line.substring(16,16+i);
	            	   int k=line.substring(16+i+2).indexOf(';');
	            	   name=line.substring(16+i+2,16+i+1+k);
	            	   int j=line.substring(16+i+1+k+2).indexOf(']');
	            	   kinetics=line.substring(16+i+1+k+3,16+i+1+k+2+j);	            	 
	            	   ((ParamTableEvents)eventValues).getEventsModel().addEvent(((ParamTableEvents)eventValues).getEventsModel().new Event(null,condition,name,kinetics));
	            	   for(int c=0;c<((ParamTableEvents)eventValues).getEventsModel().getViews().size();c++){
	            		   ((ParamTableEvents)eventValues).getEventsModel().getViews().get(c).refresh();
	            	   }
	            	   
	               }else if (line.startsWith("[GUI] add_time_event")) {// set the events
	            	   	            	   
	            	   String condition,name,kinetics,time;		    
	            	   int i = line.substring(21).indexOf(';');
	            	   time=line.substring(21,21+i);	            	   
	            	   int k=line.substring(21+i+2).indexOf(';');
	            	   condition=line.substring(21+i+1,21+i+2+k);
	            	   int j=line.substring(21+i+2+k+2).indexOf(';');
	            	   name=line.substring(21+i+2+k+2,21+i+2+k+1+j);
	            	   int l=line.substring(21+i+2+k+j+3).indexOf(']');
	            	   kinetics=line.substring(21+i+2+k+j+4,21+i+2+k+j+3+l);	            	   
	            	   ((ParamTableEvents)eventValues).getEventsModel().addEvent(((ParamTableEvents)eventValues).getEventsModel().new Event(time,condition,name,kinetics));
	            	   for(int c=0;c<((ParamTableEvents)eventValues).getEventsModel().getViews().size();c++){
	            		   ((ParamTableEvents)eventValues).getEventsModel().getViews().get(c).refresh();
	            	   }
	            	   
	               }else if(line.startsWith("[GUI] undefined") && model.isCommandLineMode()){
	            	   String value=line.substring(16);
	            	   int index=((ParamTableInitConc)initVal).indexOf(value);
	            	   if(index>=0){
	            		   ((ParamTableInitConc)initVal).getInitConcentrations().remove(index);
	            		   ((ParamTableInitConc)initVal).removeUndefinedFromGUI(value);
	            	   }
	            	   
	            	   
	               }else if(line.startsWith("[GUI] satisfactionDegree")){
	            	   ParamTableLTLSpecifications ltl_check=(ParamTableLTLSpecifications)ltlSpecifications;
		               JPanel pan=ltl_check.getLTLModelChecking();
		               Component[] comps=pan.getComponents();
		               int size=comps.length;	
		               String message=line.substring(25);
		               for(int i=0;i<size;i++){
		            	   if(comps[i] instanceof JTextArea){
		            		   JTextArea ta=(JTextArea)comps[i];
		            		   if(ltl_check.isFromBeggining()){		            			 
		            			   ltl_check.setFromBeggining(false);
		            		   }
		            		  if(!message.equals(tempString)){
		            			   ta.append("\n"+" "+message);
		            		  }else{
		            			  cnt++;		            			  
		            		  }
		            		  if(cnt>5){
		            			  ta.append("In progress....Please wait.....\n");
		            			  cnt=0;
		            		  }		            		   
		            	   }
		               }		              
		               tempString=message;
		               //pan.validate();
		               pan.repaint();
		               pan=null;
		               comps=null;
		               ltl_check=null;
	               }             
	               else if (line.startsWith("[GUI] initv ")) { // set the initial values
	            	   
	            	   
	            	   int i = line.substring(12).lastIndexOf(',');
	            	   String p = line.substring(12,12+i); 
	            	   String v = line.substring(12+i+1);
	            	   if(!((ParamTableInitConc)initVal).isNotToAdd()){
	            		   arguments.clear();
	            		   if (v.matches("\\d+")){		
	            			   ((ParamTableInitConc)initVal).getInitStateModel().addInitState(p, v+".0");
			            	   for(int c=0;c<((ParamTableInitConc)initVal).getInitStateModel().getViews().size();c++){
			            		   ((ParamTableInitConc)initVal).getInitStateModel().getViews().get(c).refresh();
			            	   }
	            			  /* arguments.add(p);
	            			   arguments.add(v+".0");			                	  
	            			   initVal.setValue(arguments);
	            			   for(int c=0;c<((ParamTableInitConc)initVal).getInitStateModel().getViews().size();c++){
			            		   ((ParamTableInitConc)initVal).getInitStateModel().getViews().get(c).refresh();
			            	   }
	            			   arguments.clear();  */               	 
	            		   }else{
	            			   ((ParamTableInitConc)initVal).getInitStateModel().addInitState(p, v);
			            	   for(int c=0;c<((ParamTableInitConc)initVal).getInitStateModel().getViews().size();c++){
			            		   ((ParamTableInitConc)initVal).getInitStateModel().getViews().get(c).refresh();
			            	   }
	            			  /* arguments.add(p);
	            			   arguments.add(v);			            			   
	            			   initVal.setValue(arguments);
	            			   for(int c=0;c<((ParamTableInitConc)initVal).getInitStateModel().getViews().size();c++){
			            		   ((ParamTableInitConc)initVal).getInitStateModel().getViews().get(c).refresh();
			            	   }
	            			   arguments.clear();			     */           	 	                	  
	            		   }
	            	   }else{
	            		   // just internal into Biocham,and not to the graphical interface....
	            		   ((ParamTableInitConc)initVal).setNotToAdd(false);
	            	   }
	        
	            	   
	            	   
	            	   
	            	   
	           
	               }else if (line.startsWith("[GUI] checkMolecule")) { // check if the list of molecules is updated
	            	
	            	   
		               String message=line.substring(21);
		               ParamTableMolecules ptm=(ParamTableMolecules)molecules;
		               if(message.contains("[GUI] warnings")){
		            	   message="Warning: "+message.substring(15);
		               }
		               if(!ptm.getCheckMolecules().contains(message)){
		            	   ptm.getCheckMolecules().add(message);
		               }		               	             
		               ptm=null;            
		               
		               
		               
		               
	               }else if (line.startsWith("[GUI] show ")) { // send a commend back to biocham to show/hide the chosen molecules from the plot
	            	   
	            	              	   
	            	   
	            	   JDialog showDialog = new JDialog(BiochamMainFrame.frame,true);
	            	   showDialog.setTitle("Shown molecules");
	            	   showDialog.setLayout(new GridLayout(0,3));
	            	   
	            	   ActionListener al = new ActionListener () {
	            		   public void actionPerformed(ActionEvent e) {
	            			   JCheckBox jcb = (JCheckBox) e.getSource();
	            			   if (jcb.isSelected()) {
	            				   biochamInput.append("show");
	            			   }else {
	            				   biochamInput.append("hide");
	            			   }		                     
	            			   biochamInput.append("_molecules("+jcb.getText()+").\n");
	            		   }
	            	   };
	            	   
	            	   int nbMol=0;
	            	   int maxMol=0;
	            	   while (!(line = biochamOutput.readLine()).startsWith("[GUI] hide")) {
	            		   nbMol++;
	            		   if (line.length()>maxMol)
	            			   maxMol = line.length();
	            		   JCheckBox jcb = new JCheckBox(line.substring(5),line.startsWith("show "));
	            		   jcb.addActionListener(al);
	            		   showDialog.add(jcb);
	            	   }
	            	   Dimension dim = new Dimension((maxMol-5)*24+60,(((nbMol-1)/3)+3)*20);
	            	   showDialog.setSize(dim);
	            	   Point pos = BiochamMainFrame.frame.getLocationOnScreen();
	            	   showDialog.setLocation( pos.x+BiochamMainFrame.frame.getSize().width/2-dim.width/2, pos.y+BiochamMainFrame.frame.getSize().height/2-dim.height/2);
	            	   showDialog.setResizable(true);
	            	   showDialog.setVisible(true);
	            	   
	            	   
	               }else if(line.startsWith("[GUI] showKinetics")){  
	            	   String txt=line.substring(18);
	            	 
	            	   if(txt.contains("Start")){
	            		   if(model.getKinetics()!=null){
	            			   model.getKinetics().clear();	            			   
	            		   }else{
	            			   model.setKinetics(new ArrayList<Object[]>());
	            		   }
	            	   }else if(txt.contains("Finish")){
	            		   /*model.getKineticsDialog().populateTable(model);
	            		   ((ParamTableRules)model.getRules().getParamTable()).setSplitPane(model.getKineticsDialog().getKineticsTable());
	            		   if(model.isLoadingModel()){
	            			   model.setLoadingModel(false);		            		  
		            		   model.stopProgressBar();
		        			   WorkbenchArea.removeTab("Loading Model in Progress");        			   
		        			   if(WorkbenchArea.tabbedPane.getTabCount()<1 && model!=null){
		        					WorkbenchArea.tabbedPane.add("Rules",((ParamTableRules)model.getRules().getParamTable()).getSplitPane());
		        				}
	            		   }*/
	            		   if(model.isLoadingModel()){
	            			   model.setLoadingModel(false);
	            			  /* model.setLoadingModel(false);		            		  
		            		   model.stopProgressBar();
		        			   WorkbenchArea.removeTab("Loading Model in Progress");*/
		        			   
		        			   Utils.debugMsg("UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU33333333333333");
		        			   ((ParamTableRules)model.getRules().getParamTable()).refreshAfterAddingNew();
		        			 //  ((ParamTableRules)model.getRules().getParamTable()).refreshAfterAddingNew();		
		        			  // ((ParamTableRules)model.getRules().getParamTable()).setPanelPreferredSize();//.getPanel().setPreferredSize(new Dimension(((ParamTableRules)model.getRules().getParamTable()).getPanelCurrentX(),((ParamTableRules)model.getRules().getParamTable()).getPanelCurrentY()));
		        			  // ((ParamTableRules)model.getRules().getParamTable()).getPanel().revalidate();
		        			   
		        			  /* if(BiochamDynamicTree.getCorrespondigTabbedPane().getTabCount()<1 && model!=null){
		        				   BiochamDynamicTree.getCorrespondigTabbedPane().add("Reactions",model.getRules().getParametersPanel());
		        				}*/
	            		   }
	            		   model.getSimulationsPanel().getPlottingPanel().rebuild();
	            	   }else{
	            		   String val=txt.substring(0,txt.indexOf(",")).trim();
	            		   double valI=0;
	            		   try{
	            			   valI=Double.parseDouble(val);
	            		   }catch(Exception e){
	            			   Utils.debugMsg("Kinetics "+val+" has incorrect value...."+valI+"?");
	            			   WorkbenchToolBars.infoLabel.setText("Kinetics "+val+" has incorrect value...."+valI+"? Value set to default(0).");
	            			   WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);
	            		   }
	            		   
	            		   val=txt.substring(txt.indexOf(",")+1);
	            		   model.getKinetics().add(new Object[]{valI,val});	            		   
	            	   }
	            	   
	            	   
	               }else if(line.startsWith("[GUI] list_dimensions")){
	            	   ((AbstractionView)model.getWhoPopup()).appendOutput(line.substring(21));   
	            	   
	               }else if(line.startsWith("[GUI] list_influences")){
	            	   ((AbstractionView)model.getWhoPopup()).appendOutput(line.substring(21));	            	   
	               }else if(line.startsWith("[GUI] list_neighborhood")){
	            	   ((AbstractionView)model.getWhoPopup()).appendOutput(line.substring(23));
	               }else if(line.startsWith("[GUI] set_dimension")){
	            	   //set_dimension kdDNA;- 2
	            	   String n=line.substring(20,line.indexOf(";"));
	            	   String v=line.substring(line.indexOf(";")+1);
	            	   Utils.debugMsg("setting dimension for "+n+": "+v+".");
	               }else if(line.startsWith("[GUI] list_functions")){
	            	   
	            	   ((AbstractionView)model.getWhoPopup()).appendOutput(line.substring(20));
	            	   
	               }else if (line.startsWith("[GUI] quit")) { // if the BIOCHAM is closed, ensure to close the GUI also
	            	   
	            	   
	            	   model.disposeBiochamModel();
 	            	    
	              
	            	   
	            	   
	               
	               }else if (line.startsWith("[GUI] clear_initial_state")) { // clear the screens of parameters and init values
	            	               	   
	            	   ((ParamTableInitConc)initVal).getInitStateModel().deleteAll();
	            	   model.sendToBiocham("list_molecules.\n");
	            	   // TO BE DONE!!!!!
	            	   // param.clearParamScreen();
	                   //initVal.clearParamScreen();
	               
	               
	            	   
	               
	               }else if(line.startsWith("Value of type")){
	            	   
	            	   
	            	   JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Value of type <name> expected. ");
	            	   checkCorrectness(line);
	                            	
	    
	            	   
	            	   
	            	   
	            	   
	               }else if(line.startsWith("[GUI] warnings") || line.contains("arning")){	            	  
	            	                		            		
		            	if(!((ParamTableCTLSpecifications)ctlSpecifications).isRevising()){
		            		
		            		model.getModelWarnings().append(Color.GREEN.darker(), "Warning: ");
		            		model.getModelWarnings().append(Color.BLACK,line.substring(15)+"\n");
		            		try{
		            			WorkbenchToolBars.infoLabel.setText(line.substring(15));
		            			WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);
		            		}catch(Exception e){}
		            		
		            		//JOptionPane.showMessageDialog(BiochamMainFrame.frame,line.substring(15));
		            	  	
		            	}else{
		            		
		            		if(!model.getModelWarnings().getTarea().getText().contains(line.substring(15))){
		            			model.getModelWarnings().append(Color.GREEN.darker(), "Revising Warning: ");
		            			model.getModelWarnings().append(Color.BLACK,line.substring(15)+"\n"); 
			            		
		            		}
		            	}
	            	   
	            	   
	            	   
		            	
	               }
	               else if(line.startsWith("[GUI] errors")  || line.contains("rror ")){
	            	   	   
	            	   if(!line.substring(13).contains("instantiation_error")){	            		   
	            		   
	            		   
	            		   model.getModelWarnings().append(Color.RED.darker(), "Error: ");
	            		   model.getModelWarnings().append(Color.BLACK,line.substring(13)+"\n"); 
	            		   try{
		            			WorkbenchToolBars.infoLabel.setText(line.substring(13));
		            			WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);
		            		}catch(Exception e){}
	            		  // JOptionPane.showMessageDialog(BiochamMainFrame.frame, line.substring(13),"Error",JOptionPane.ERROR_MESSAGE);   
	            		   
		            	   /*if(!line.substring(13).contains("Aborting evaluation")){
		            		  		            		  
		            		   			            	  
		            	   }*/
		            	   
		            	   checkCorrectness(line.substring(13));
		            	  
		            	  // Parser_SBGNRule2BiochamRule.ruleValidationPassed=false;
		            	   ReactionDialog.removeTemporalGraphObjects();
		            
	            	   }
	            	   
	            	   
	            	   String who=model.getWho();
	            	   if(who!=null){
		            	   if(who.equals("LTL")){
		            		   //((ParamTableLTLSpecifications)ltlSpecifications).getTarea().append(" Error: "+line.substring(13)+"\n Command stopped.\n");
		            		   ((ParamTableLTLSpecifications)ltlSpecifications).getLtlModel().getViews().get(0).appendResults(" Error: "+line.substring(13)+"\n Command stopped.\n");
		            	   }else if(who.equals("CTL")){
		            		   ((ParamTableCTLSpecifications)ctlSpecifications).getCtlModel().getViews().get(0).appendResults(" Error: "+line.substring(13)+"\n Command stopped.\n");
		            		   //((ParamTableCTLSpecifications)ctlSpecifications).getTarea().append(" Error: "+line.substring(13)+"\n Command stopped.\n");
		            	   }else if(who.contains("conservation")){
		            		   ((ParamTableConservationLaws)conservationLaws).getTarea().append(" Error: "+line.substring(13)+"\n Command stopped.\n");
		            	   }
	            	   }
	            	   who=null;
	            	   if(line.contains("Syntax error") || line.contains("Lexing error")  || line.contains("character")){
		            	   
		            	   
		            	   	String message=line.substring(12);
		            	   
			            	 
			            	if(!line.contains("character")){
			            		JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You've had a syntax/lexing error. "+message);
			            		model.getModelWarnings().append(Color.RED.darker(), "Error: ");
			            		model.getModelWarnings().append(Color.BLACK,message+"\n");
			            	}else{
			            		if(!lineBefore.contains("command")){
			            			model.getModelWarnings().append(Color.RED.darker(), "Syntax Error: ");
					            	
					            	if(lineBefore.contains("expected")){
					            		model.getModelWarnings().append(Color.BLACK,message+": ");
					            		model.getModelWarnings().append(Color.black, lineBefore.substring(5)+" Appeared just after: ");					            		
					            		model.getModelWarnings().append(Color.blue,lineBBefore.substring(5)+"\n");
					            		
					            	}/*else if(lineBefore.contains("Uknown command")){
					            		w.append(Color.black, lineBefore.substring(5,lineBefore.lastIndexOf(":")+1));
					            		w.append(Color.blue, lineBefore.substring(lineBefore.lastIndexOf(":"))+". \n");
					            	}*/else{
					            		model.getModelWarnings().append(Color.BLACK,message+",just after: ");
					            		model.getModelWarnings().append(Color.blue,lineBefore.substring(5)+"\n");
					            	}
			            		}
				            	
			            	}
		            	   	
		            	    checkCorrectness(line.substring(12));
		       
		            	   	
		            	   	
		            	   	
		               }/*else if(line.contains("command")){
		            	   String message=line.substring(12);		            	 
		            	   BiochamWarnings w=model.getModelWarnings();
		            	   
		            	   w.append(Color.RED.darker(), "Error: ");
		            	   w.append(Color.BLACK,message.substring(0,message.lastIndexOf(":")));		            	
		            	   w.append(Color.blue, message.substring(message.lastIndexOf(":"))+"\n");
		               }*/
	            	   
	            	   
	               }else if(line.contains("Syntax error") || line.contains("Lexing error")){
	            	   
	            	   
	            	   	String message=line.substring(12);
	            	   	JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You've had a syntax/lexing error. "+message);
	            	    checkCorrectness(line.substring(12));
	       
	            	   	
	            	   	
	            	   	
	               }else if(line.startsWith("error(syntax_error") ){
	            	   
	            	   
	          	  	   String message=line.substring(18);
	         	   	   JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You had a syntax error. "+message);
	         	   	   checkCorrectness(line.substring(18));
	       
	         	   	   
	         	   	   
	         	   	   
	       
	               }else if(line.startsWith("[GUI] warnings There were")){
	            	   
	            	   ((ParamTableRules)rules).setIgnoreUndefinedParametersWarnings(false);
	            	   
	               }/*else if(line.startsWith("fail")){
	            	
	            	   	String message=line.substring(4);
	            	   	JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Biocham crossed an error and failed. "+message);
	            	   	line=null;
	            	   	eof=true;
	            	   	int index=model.getModelNodeIndex();
	            	   	WorkbenchArea.tree.removeNode(index);	            	   
	            	            	   
	            	   
	            
	               
	               
	               }*/else {
	            	
	            	   	            	   
	            	   if (line.startsWith("** There were ")) //beep if the response of BIOCHAM starts with the string "** There were errors.."
	            		   Utils.debugMsg("\n\n\n** There were \n");
	            		  // Toolkit.getDefaultToolkit().beep();
	                
	            	   
	            	   
	               }
	        		
	        		 if(line.startsWith("[GUI]") && !line.contains("errors")){
	        			 lineBBefore=lineBefore;
	        			 lineBefore=line;
	        		 }
	        		
	        	 }else{
	        		 sleep(100);
	        	 }
	        	
	        	 
	        	 
	        	 
	         } catch(InterruptedIOException e) {
				Thread.currentThread().interrupt();
			 }
			 catch (EOFException e) {
	        	 
	        	 Utils.debugMsg("\n\n\nERROR_5\n\n");
	        	 e.printStackTrace();        	 
	        	 eof = true;
	        	 if(biochamOutput!=null){
	        		 Utils.debugMsg("\n\n\nERROR_5\n\n");
		      		 try {
		      			 biochamOutput.close();
		      			 Utils.debugMsg("\n\n\nERROR_6\n\n");
		      		 } catch (IOException e1) {

		      			 // TO BE DONE!!!!!!!!!
		      			 Utils.debugMsg("\n\n\nERROR_7\n\n");
		      			 e1.printStackTrace();
		      		 }
	        	 }
	        	 
	        	 
	         } catch (Exception e) {
	        	 
	        	 Utils.debugMsg("\n\n\nERROR_8\n\n");
	        	 e.printStackTrace();
	     
	         }
	      
		
		
		}// END OF THE WHILE LOOP
	      
	     	      
	}// END OF THE RUN METHOD


	private boolean containsReactionShortcut(String p) {
		if(p.contains("complexation") || p.contains("decomplexation") || p.contains("re_complexation") || p.contains("phosphorylation") || p.contains("dephosphorylation") || p.contains("re_phosphorylation") || 
				p.contains("synthesis") || p.contains("degradation") || p.contains("elementary")){
			return true;
		}else{
			return false;
		}
	}


	/**
	 * @param line
	 */
	private void checkCorrectness(String line) {
		if(line.contains("ltl")){
			   
			   if(((ParamTableLTLSpecifications)ltlSpecifications).isModifying()){
				   ((ParamTableLTLSpecifications)ltlSpecifications).removeLastAdded();
				   ((ParamTableLTLSpecifications)ltlSpecifications).setModifying(false);
			   }
			   
		   }else if(line.contains("biocham_query")){
			 
			   if(((ParamTableCTLSpecifications)ctlSpecifications).isModifying()){
				   ((ParamTableCTLSpecifications)ctlSpecifications).removeLastAdded();	            		   
				   ((ParamTableCTLSpecifications)ctlSpecifications).setModifying(false);
			   }
		   }else{
			   
			  
			   if(((ParamTableCTLSpecifications)ctlSpecifications).isModifying()){
				   ((ParamTableCTLSpecifications)ctlSpecifications).removeLastAdded();	            		   
				   ((ParamTableCTLSpecifications)ctlSpecifications).setModifying(false);
			   }else if(((ParamTableLTLSpecifications)ltlSpecifications).isModifying()){
				   ((ParamTableLTLSpecifications)ltlSpecifications).removeLastAdded();
				   ((ParamTableLTLSpecifications)ltlSpecifications).setModifying(false);
			   }else if(((ParamTableDeclarations)declarations).isModifying()){
				   ((ParamTableDeclarations)declarations).removeLastAdded();
				   ((ParamTableDeclarations)declarations).setModifying(false);
			   }else if(((ParamTableParameters)param).isModifying()){
				   ((ParamTableParameters)param).removeLastAdded();
				   ((ParamTableParameters)param).setModifying(false);
			   }else if(((ParamTableInitConc)initVal).isModifying()){
				   ((ParamTableInitConc)initVal).removeLastAdded();
				   ((ParamTableInitConc)initVal).setModifying(false);
				   
			   }		            		   
			   else if(((ParamTableVolumes)volumes).isModifying()){
				   ((ParamTableVolumes)volumes).removeLastAdded();
				   ((ParamTableVolumes)volumes).setModifying(false);
			   }else if(((ParamTableEvents)eventValues).isModiff()){
				   ((ParamTableEvents)eventValues).removeLastAdded();
				   ((ParamTableEvents)eventValues).setModiff(false);
			   }else if(((ParamTableMacros)macros).isModiff()){
				   ((ParamTableMacros)macros).removeLastAdded();
				   ((ParamTableMacros)macros).setModiff(false);
			   }
		   }
	}
	
	
	
	
	/**
	 * Cleans up the ressources, disposes the references to null.
	 */

	public void disposeFields() {
		
		eof=true;
		//model.setPauseLoggerThread(true);
		arguments.clear();
		arguments=null;
		model=null;
	    param = null; 
	    initVal = null; 
	    eventValues=null;
	    rules=null;
	    ctlSpecifications=null;
	    ltlSpecifications=null;
	    declarations=null;
	    macros=null;
	    molecules=null;
	    volumes=null;
	    plotData.clear();
	    plotData=null;	   
		
	}
	
	/**
	 * Returns the set of model's plot's data
	 */
	public HashMap<String, String> getPlotData() {
		return plotData;
	}

	/**
	 * Sets a reference to the set of model's plot's data
	 */
	public void setPlotData(HashMap<String, String> plotData) {
		this.plotData = plotData;
	}

	/**
	 * Checks if the communication thread has to finish its execution or not
	 */
	public boolean isEof() {
		return eof;
	}

	/**
	 * Sets the communication thread execution on or off.
	 * 
	 * @param eof True means that the communication tread has to finish. False means that the thread has to continue. 
	 * 
	 */
	public void setEof(boolean eof) {
		this.eof = eof;
	}


	public boolean isCmaesPlot() {
		return cmaesPlot;
	}


	public void setCmaesPlot(boolean cmaesPlot) {
		this.cmaesPlot = cmaesPlot;
	}


	public boolean isTraceAnalyze() {
		return traceAnalyze;
	}


	public void setTraceAnalyze(boolean traceAnalyze) {
		this.traceAnalyze = traceAnalyze;
	}


	public String getInputFromGUI() {
		return inputFromGUI;
	}


	public void setInputFromGUI(String inputFromGUI) {
		this.inputFromGUI = inputFromGUI;
	}


	public void interrupt() {
		super.interrupt();
		try {
			biochamInput.close();
			biochamOutput.close();
		} catch (IOException e) {}
	}
	
}
