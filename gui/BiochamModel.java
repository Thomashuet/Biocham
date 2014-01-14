package fr.inria.contraintes.biocham;

import fr.inria.contraintes.biocham.commandLine.SimpleCommandLine;
import fr.inria.contraintes.biocham.customComponents.BiochamFrame;
import fr.inria.contraintes.biocham.customComponents.BiochamPlot;
import fr.inria.contraintes.biocham.customComponents.BooleanPlot;
import fr.inria.contraintes.biocham.customComponents.CustomZoomableImagePanel;
import fr.inria.contraintes.biocham.customComponents.PlotImage;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.dialogs.DialogChooseOptions;
import fr.inria.contraintes.biocham.dialogs.DialogKinetics;
import fr.inria.contraintes.biocham.graphicalEditor.BiochamGraph;
import fr.inria.contraintes.biocham.graphicalEditor.BiochamGraphEditorDesktop;
import fr.inria.contraintes.biocham.graphicalEditor.Parser_BiochamRule2SBGNRule;
import fr.inria.contraintes.biocham.modelData.AbstractionModel;
import fr.inria.contraintes.biocham.modelData.AbstractionView;
import fr.inria.contraintes.biocham.modelData.IView;
import fr.inria.contraintes.biocham.modelData.ParamTableCTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableDeclarations;
import fr.inria.contraintes.biocham.modelData.ParamTableEvents;
import fr.inria.contraintes.biocham.modelData.ParamTableInitConc;
import fr.inria.contraintes.biocham.modelData.ParamTableLTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableMacros;
import fr.inria.contraintes.biocham.modelData.ParamTableMolecules;
import fr.inria.contraintes.biocham.modelData.ParamTableParameters;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.modelData.SimulationModel;
import fr.inria.contraintes.biocham.modelData.SimulationView;
import fr.inria.contraintes.biocham.modelData.ParamTableVolumes;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SupportedSuffixes;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.Color;
import java.awt.Font;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Vector;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.tree.DefaultMutableTreeNode;



/**
 * Class that represents a model in Biocham.   
 * 
 * @author Dragana Jovanovska  
 */ 

public class BiochamModel {
	
	
	
	static final int LEFT_OFF = 5;
	public static final int TOP_OFF = 15;
	public static final int HEIGHT = 25;
	static final int RIGHT_OFF = 350;
	public static final int MIDDLE = 10;
	private static int index=-1;	
	private String modelName;		
	private String arguments;	
	private String modelToolTip;	
	private String biochamExecutable;
	private String modelFileFromBackup;
	private File plotFile, influencesImageFile, reactionImageFile,modelFile=null, modelBackupFile=null;
	public HashMap<String,String> simulationMap;   
	private UniversalScrollPane reactionGraph, influencesGraph;
	private DefaultMutableTreeNode warningsParent;
	private Process biochamProcess;
	private ArrayList<File> addedFiles;
	private ArrayList<PlotImage> plotImages;
	private int modelNodeIndex;	
	private int connectionNumber=0;
	private boolean enableExport=false;	
	private boolean pauseLoggerThread=false;
	private boolean commandLineMode=true;	
	private boolean draggedAndDropped=false;
	private boolean saved=false;
	private String who=null;
	private BiochamDynamicTree treeInstance;
	BiochamGraphEditorDesktop graphEditor;
	BiochamGraph reactionsGraphEditor;
	BiochamPlot currentPlot;
	
	int lastSimLenght;
	Vector<String> historyVector;
	int historyIndex;
	
	HashMap<String, String> plotData;	
	ArrayList<Object[]> kinetics;
	JPanel newPlotTab;
	private int odeCounter=0,stochCounter=0,boolCounter=0, rgCounter=0,igCounter=0, nhCounter=0;;
	private String simulationType="";
	public HashMap<String,BiochamPlot> odePlots;
	public HashMap<String,BooleanPlot> booleanPlots;
	private boolean loadingModel=false;
	int counter=0;
	public Parser_BiochamRule2SBGNRule parserBcRule2Graph;	
	public DialogKinetics kineticsDialog;	
	BiochamModelEditor modelEditor;
	JTextArea initModelFileView;
	JScrollPane fileModelViewSP;
	String tabTitle;
	boolean existingModel;
	//UniversalScrollPane simulationsScrollPane;
	//JDialog rulesD,initConcD,conLawsD,paramD,eventsD,macrosD,declD,volumeD,molsD;
	boolean rulesD,initConcD,conLawsD,paramD,eventsD,macrosD,declD,volumeD,molsD;	
	public BiochamFrame rulesF,initConcF,conLawsF,paramF,eventsF,macrosF,declF,volumeF,molsF,
						booleanTPF, numericalTPF, abstractionsF,simulationF;
	BiochamFrame graphEditorFrame;
	public BiochamFrame popupFrames[];
	ArrayList<IView> modelViews;	
	SimulationView simulationsPanel;
    IView whoPopup,whoPopupSimulation;


	/**
	 * Input for Biocham
	 */
	//input for the BioCham process...
	private PrintStream biochamInput;
	
	/**
	 * Output from Biocham
	 */
	//response of the Biocham process...
	private BufferedReader biochamOutput;
	
	private BiochamModelElement parameters,
								initConditions,
								rules,
								rulesEditor,
								ctlSpecifications,
								ltlSpecifications,
								declarations,
								events,
								macros,
								molecules,
								volumes,
								conservationLaws,
								modelData,
								abstractions;
	
	private BiochamModelElement[] modelElements;
	private BiochamLoggerThread loggerThread;
	private BiochamWarnings modelWarnings;
	private SimpleCommandLine commandLine;
	private CustomZoomableImagePanel rGraph,iGraph;
	private BiochamModel model;	
	public JPanel warningsPanel;	
	AbstractionModel abstractionModel;
	SimulationModel simulationModel;
	// Constructor
	AbstractionView abstractionView;
	





	/**
	 * It initialize the model from a file or an empty one
	 * @param o It's a file if we want to initialize the model from a file, otherwise it's null	
	 */
	public BiochamModel(final Object o,BiochamDynamicTree tree){
			
		
		boolean b=SwingUtilities.isEventDispatchThread();

		if(o instanceof File){
			initModelFileView=new JTextArea();
			initModelFileView.setEditable(false);
			initModelFileView.setForeground(Color.black);		
			initModelFileView.setFont(new Font(initModelFileView.getFont().getFontName(),Font.BOLD,initModelFileView.getFont().getSize()+2));
			fileModelViewSP=new JScrollPane(initModelFileView);
			fileModelViewSP.setAutoscrolls(true);
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					FileReader fr = null;
					try {
						
						fr = new FileReader(((File)o).getAbsolutePath());				
						initModelFileView.read(fr, null);
						fr.close();
						
					} catch (FileNotFoundException e) {
						e.printStackTrace();
					} catch (IOException e) {
						e.printStackTrace();
					}finally{
						fr=null;
					}		
					return null;
				}

				@Override
				public void finished() {
					StringBuilder sb=new StringBuilder();
					sb.append("  Initial Model View - ");
					sb.append(((File)o).getName());
					tabTitle=sb.toString();
					setInitFileModelTab();
					sb=null;
				}};
			sw.start();
				

		}

		
		final BiochamModel m=this;
		setTreeInstance(tree);
		tree.currentModel=this;
		Utils.debugMsg("\n\n\n\nBiochamModel: Set CurrentModel="+tree.currentModel.getModelName()+"\n\n\n\n");
		addedFiles=new ArrayList<File>();
	
		plotImages=new ArrayList<PlotImage>();
		plotData=new HashMap<String,String>();
		if(o instanceof File){
			modelFromFile((File)o);		
		}else if(o instanceof Integer){
			emptyModel((Integer)o);
		}		
		graphEditor=new BiochamGraphEditorDesktop(this);
		setGraphEditor(graphEditor);
		setModelToolTip(getModelName());
		setModel(this);	
		odePlots=new HashMap<String,BiochamPlot>();
		booleanPlots=new HashMap<String,BooleanPlot>();
		parserBcRule2Graph=new Parser_BiochamRule2SBGNRule(this);
		
		kineticsDialog=new DialogKinetics();  
		enableExport=true;
		saved=true;
		
		
		initConcF=new BiochamFrame("Initial State - "+getModelName());
		rulesF=new BiochamFrame("Reaction Rules - "+getModelName());
		conLawsF=new BiochamFrame("Conservation Laws - "+getModelName());
		paramF=new BiochamFrame("Parameters - "+getModelName());
		eventsF=new BiochamFrame("Events - "+getModelName());
		macrosF=new BiochamFrame("Macros - "+getModelName());
		declF=new BiochamFrame("Declarations - "+getModelName());
		volumeF=new BiochamFrame("Volumes - "+getModelName());
		molsF=new BiochamFrame("Molecules - "+getModelName());
		
		booleanTPF=new BiochamFrame("Boolean temporal Properties - "+getModelName());
		
		numericalTPF=new BiochamFrame("Numerical Temporal Properties - "+getModelName());
		abstractionsF=new BiochamFrame("Abstractions - "+getModelName());
		simulationF=new BiochamFrame("Simulations - "+getModelName());
		
		popupFrames=new BiochamFrame[13];
		popupFrames[0]=rulesF;
		popupFrames[1]=conLawsF;
		popupFrames[2]=paramF;
		popupFrames[3]=initConcF;
		popupFrames[4]=eventsF;
		popupFrames[5]=macrosF;
		popupFrames[6]=declF;
		popupFrames[7]=volumeF;
		popupFrames[8]=molsF;
		
		popupFrames[9]=booleanTPF;
		popupFrames[10]=numericalTPF;
		popupFrames[11]=abstractionsF;
		popupFrames[11]=simulationF;
		
		modelViews=new ArrayList<IView>(12);
		
		abstractionModel=new AbstractionModel(this);
		abstractionView=new AbstractionView(BiochamMainFrame.frame,this);	
		simulationModel=new SimulationModel(this);
		
		simulationMap = new HashMap<String,String>();
	}
	
	
	
	
	
	// Model's name generation
	
	

	/**
	 * Sets a name to the model, depending if it's an existing model or a new empty one.
	 * Sets the filename as a model's name
	 */	
	public void modelFromFile(File f){
		
		setModelName(f.getName());
		if(f!=null){
			setModelFile(f);
			setSaved(true);
			setEnableExport(true);
			setExistingModel(true);
		}		
	}
	
	
	/**
	 * Sets a name to the model, depending if it's an existing model or a new empty one.
	 * Generates and sets a name to the model as concatanation of "New Biocham" and the model's index in the Workbench.
	 */
	public void emptyModel(int i){
		setModelName("New Biocham "+i);
		setExistingModel(false);
		setSaved(true);
		setEnableExport(true);
	}
	
	 
	
	
	
	
	
	
	// Model's structure initialization and connection to Biocham
	/**
	 * Initializes the model's structure and connection to Biocham environment
	 */	  
	public void initializeModelElements(DefaultMutableTreeNode parent,WorkbenchArea workbench, File file) {
				
		events = new BiochamModelElement(parent,workbench,"Events");	
		initConditions = new BiochamModelElement(parent,workbench,"Initial State");
		molecules = new BiochamModelElement(parent,workbench,"Molecules");
		volumes = new BiochamModelElement(parent,workbench,"Volumes");
		declarations = new BiochamModelElement(parent,workbench,"Declarations");	
		parameters = new BiochamModelElement(parent,workbench,"Parameters"); 			
		macros = new BiochamModelElement(parent,workbench,"Macros");
		conservationLaws=new BiochamModelElement(parent,workbench,"Conservation Laws");
		rules = new BiochamModelElement(parent,workbench,"Reaction Rules");		
		rulesEditor = new BiochamModelElement(parent,workbench,"Reaction Graph Editor");	
		modelData = new BiochamModelElement(parent,workbench,"Reaction Rule Model");
		
		simulationsPanel=new SimulationView(BiochamMainFrame.frame,this);
		//simulationsScrollPane=new UniversalScrollPane(simulationsPanel);		
		WorkbenchArea.tree.addNodeObject(parent,simulationsPanel);
		
		
		ctlSpecifications = new BiochamModelElement(parent,workbench,"Boolean Temporal Properties");
		ltlSpecifications = new BiochamModelElement(parent,workbench,"Numerical Temporal Properties");
		
		
		WorkbenchArea.tree.addNodeObject(parent,abstractionView);
		
		
		parent=WorkbenchArea.tree.addNodeObject(parent,new BiochamDynamicTreeSeparator(this,""));
		modelWarnings=new BiochamWarnings(parent,this);//getWarningsParent()
		commandLine=new SimpleCommandLine(parent,this);		//getWarningsParent()
		modelElements=new BiochamModelElement[9];						
		modelElements[0]=rules;		
		modelElements[1]=initConditions;
		modelElements[2]=parameters;		
		modelElements[3]=declarations;
		modelElements[4]=events;
		modelElements[5]=macros;
		modelElements[6]=molecules;
		modelElements[7]=volumes;
		modelElements[8]=conservationLaws;		
		
		modelEditor=new BiochamModelEditor(model);		
		model.setModelEditor(modelEditor);
		setModel(this);			
		
		// connection to Biocham environment		
		loggerThread = new BiochamLoggerThread(this);	
		loggerThread.setName(getModelName());		
		loggerThread.start();
		connectToBiocham(file,false);		
		
	}
	
	
	
	/**
	 * Tries to connect to Biocham environment and to load an existing or empty model
	 * @param file If the file is not empty, loads an existing model, otherwise loads empty model
	 */
	public void connectToBiocham(File file,boolean restart){
	
		if(file!=null){			
			arguments=file.getAbsolutePath();		
		}	else{
			arguments="";
		}	
		if ((biochamExecutable=findExecutable()) != null) {
			createPipes(biochamExecutable,restart);
		}		
	
		if (biochamInput == null) {
			
			JOptionPane.showMessageDialog(BiochamMainFrame.frame,
	          "Cannot start the 'biocham' program. Please check your PATH.",
	          "Warning",
	          JOptionPane.WARNING_MESSAGE);
			try  {
						
				//biochamInput.close();
				//biochamOutput.close();
			} catch (Exception e) {
				Utils.debugMsg(e.getClass()+","+e.getMessage());
			}
		}		
	}	
	
	/**Tries to find the biocham executable from the system's path
	 * 
	 */
	public String findExecutable() {

		 	
		String bc1=Utils.BIOCHAM_DIR;
		if(bc1.endsWith("\\") || bc1.endsWith("/")){
			bc1=bc1.substring(0,bc1.length()-1);
		}
	
		Utils.debugMsg("Utils.BIOCHAM_DIR="+bc1);
		WorkbenchArea.app_path=bc1;
		bc1+=File.separator+"biocham";

		if (Utils.is_OS_WINDOWS){
			Utils.debugMsg("its windows");
			bc1 = bc1+".exe";   // Windows case
		}else if(Utils.is_OS_MAC){
			//bc1=bc1+".mac";
			Utils.debugMsg("its MACOSX ");        	 
        }else{
        	Utils.debugMsg("its Linux ");     
        }
		Utils.debugMsg("Biocham exe="+bc1);
		if ((new File(bc1)).exists()) {
			Utils.debugMsg(bc1 +" exists.");		        	 
	
            return bc1;
		}else{
			Utils.debugMsg(bc1 +" doesn't exist.");
			bc1=null;
			return null;
		}
		
	}
	
	/**Pipe opening with the BIOCHAM executable.
    *
    * Tries to launch connection to BIOCHAM environment, sets up the pipes and starts the BiochamLoggerThread(connection)
    */
   public void createPipes(String biocham,boolean restart) {
      
	   
		boolean b=SwingUtilities.isEventDispatchThread();
		
		
	   try {
		
		
		   String[] cmd = {biocham, "--have_gui", WorkbenchArea.app_path};
		   Utils.debugMsg("cmd="+biocham+"--have_gui" +WorkbenchArea.app_path);
		 
		   biochamProcess = Runtime.getRuntime().exec(cmd);		
		 
		   if(!restart){
			   index++;
			   setConnectionNumber(index);
		   }
		  
		   InputStreamReader isr=new InputStreamReader(biochamProcess.getInputStream());
		   biochamOutput = new BufferedReader(isr);
		  
		   OutputStream os=biochamProcess.getOutputStream();
		   biochamInput = new PrintStream(os,true);	
		
		   setBiochamInput(biochamInput);
		   setBiochamOutput(biochamOutput);
		   
		   cmd=null;
		   //isr.close();
		   //isr=null;
		   //os.close();
		  // os=null;
         
	   } catch (Exception e) {
		   JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Exception in creating pipes to biocham.exe.\n"+e.getMessage());
		   Utils.debugMsg("exception in creating the pipes to Biocham="+e.getMessage()+"\n");
	   }
	   
	  
	   StringBuffer buf=new StringBuffer();
	   if (arguments!=null && arguments.length()>0){
		   
		   if( arguments.endsWith("bc")) {			   
			   buf.append("load_biocham('");
			   buf.append(arguments);
			   buf.append("').\n");
		   }else if(arguments.endsWith("xml")){		
			   buf.append("load_sbml('");
			   buf.append(arguments);
			   buf.append("').\n");			  
		   }else if(arguments.endsWith("ode")){
			   buf.append("load_ode('");
			   buf.append(arguments);
			   buf.append("').\n");			  
		   }	   
		   buf.append("list_molecules.\n");
		   buf.append("check_molecules.\n");
		   buf.append("list_ODE.\n");

		  // sendToBiocham(buf.toString(),null);
		   biochamInput.print(buf.toString());
		   loadingModel=true;
		 		  
	   }	 
	   buf=null;
	   
   }
	
   /**
    * Closes the model's connection to Biocham environment
    */
	public void quitBiocham(){
		
		try {
			loggerThread.setEof(true);
			biochamInput.close();
			biochamOutput.close();
			biochamProcess.destroy();
			biochamInput=null;
			biochamOutput=null;				
			disposeBiochamModel();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	/*** if (historyVector.size() > 31)
            historyVector.removeElementAt(0);
         historyVector.add(s);
         output.append(s + "\n");
         input.setText(null);
         historyIndex = -1;*/
	
	/**
	 * Sends a command to Biocham communication stream
	 */
     
	private void sending(String cmd){
		
		 if(cmd.contains("list_molecules")){
			 ((ParamTableMolecules)model.getMolecules().getParamTable()).getMoleculesModel().getMolecules().clear();
			 for(int i=0;i<((ParamTableMolecules)model.getMolecules().getParamTable()).getMoleculesModel().getViews().size();i++){
				 ((ParamTableMolecules)model.getMolecules().getParamTable()).getMoleculesModel().getViews().get(i).refresh();
			 }
		 }
		
		biochamInput.print(cmd);
		 try {
			model.getCommandLine().inputToBiocham("Input from GUI: "+cmd+"\n");
			if(cmd.contains("add") || cmd.contains("parameter") || cmd.contains("delete") || cmd.contains("present") || cmd.contains("absent") || cmd.contains("declare") || cmd.contains("volume") || cmd.contains("macro") || cmd.contains("conservation") || cmd.contains("clear") || cmd.contains("set")){
				backupModel();
			}
			model.getLoggerThread().setInputFromGUI(cmd);
		} catch (BadLocationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (historyVector.size() > 31)
            historyVector.removeElementAt(0);
         historyVector.add(cmd);        
         historyIndex = -1;
	}
   public void sendToBiocham(String cmd,String who){
	
	   setWho(who);	 
	   if(cmd!=null){		   
		   sending(cmd);
		   Utils.debugMsg("Sent: "+cmd+", who="+who);		   
		   if(who!=null && who.equals("kinetics")){
			   try {
				  // Utils.debugMsg("Kinetics called, Im going to sleeeeeeeep............");
				loggerThread.sleep(1);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		   }
	   }
   }
  
   public void sendToBiocham(String cmd){
	  
	   if(cmd!=null){	
		   sending(cmd);
		  
		   Utils.debugMsg("1Sent without who: "+cmd);		   
	   }   
	   
   }
   public void sendToBiochamFromAbstractions(String cmd,IView view){
	  
	   if(cmd!=null){	
		   sending(cmd);
		   this.setWhoPopup(view);
		   Utils.debugMsg("2Sent without who: "+cmd);		   
	   }  	   
   }
   public void sendToBiochamFromsimulations(String cmd,IView view){
		  
	   if(cmd!=null){	
		   sending(cmd);
		   this.setWhoPopupSimulation(view);
		   Utils.debugMsg("3Sent without who: "+cmd);		   
	   }  	   
   }
   
   /**
	 * Saves the model to a file as a backup.
	 * 
	 */
   public void backupModel(){
	   SwingWorker sw=new SwingWorker(){

			@Override
			public Object construct() {
				File file=getModelBackupFile();
				
				String s="\n export_biocham('"+file+"').\n"; //There was a bug,I resolved it!!!!!!Biocham.pl!!!!!!!!!!!!!!!
				Utils.debugMsg(modelFileFromBackup+", command="+s);
				biochamInput.print(s);		  
				setSaved(true);
				s=null;
				file=null;
				return null;
			}
			@Override
			public void finished() {}	

		};
		sw.start();
   }
   
   
   /**
	 * Saves the model to a file.
	 * 
	 * @param exists If true, it saves the model to the same location, otherwise saves as the model to a new location.
	 * 
	 */
   public void saveToFile(final Boolean exists){
    	
	   
	   // boolean exists: If there is already file dedicated for this model,save it to that file,otherwise create new file
	   
		boolean b=SwingUtilities.isEventDispatchThread();
		SwingWorker sw=new SwingWorker(){

			@Override
			public Object construct() {
				File file=null;	   
				   if(exists){
					   
					   file=modelFile;
					   Utils.debugMsg(modelFile.toString());
					   String s="export_biocham('"+file+"').\n"; //There was a bug,I resolved it!!!!!!Biocham.pl!!!!!!!!!!!!!!!
					   biochamInput.print(s);		  
					   setSaved(true);
					   s=null;
				   }else{
					 
					    Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			   			Utils.fileChooser.setFileFilter(null);	   
						String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,SupportedSuffixes.BIOCHAM_SUFFIX);			
						if (rep!=null) {
			               file = new File(rep);
			               String filename=file.getName();
			              
			               if(file.getPath()!="" && file!=null){
			            	   setSaved(true);               
				               String s="export_biocham('"+file.getPath()+"').\n";
				               model.setModelName(filename);
				               model.setModelToolTip(model.getModelName());
				              
				               if(!file.getName().endsWith(".bc")){
				            	   String n=file.getPath()+".bc";
				            	   modelFile=new File(n);
				            	   n=null;
				               }else{
				            	   modelFile=file;
				               }
				               
				               setEnableExport(true);
				    		   biochamInput.print(s);	
				    		   s=null;
			               }
			               filename=null;
						}
						rep=null;
				   }
				   file=null;
				
			
				return null;
			}

			@Override
			public void finished() {
				// TODO Auto-generated method stub
				
			}

			

		};
		sw.start();
	   
   }
   
   /**
    * Sends a command to Biocham to refresh the list of the model's molecules
    */
   public void checkMolecules(){
    	 biochamInput.append("check_molecules.");
   }
   
   
   /**
    * Loads parameters or initial concentration from a file and adds it to the current model
    */
  public void loadParametersFromFile(File file){
	   
	   
		boolean b=SwingUtilities.isEventDispatchThread();
		
		
	   if(file!=null){
		   
		   //createModelBackup();
		   ((ParamTableRules)rules.getParamTable()).clearUknownParams();		   
		   boolean ok=true;
		   String filename=file.getName();
           int index = filename.lastIndexOf('.');
           String extension = null;
           if (index>0 && index <= filename.length() - 2 ) {
        	   extension=filename.substring(index+1);
          	 
           }		 
           StringBuffer buf=new StringBuffer();
           if(extension.contains("bc")){
        	   buf.append("add_biocham('");
        	   buf.append(file.getPath());
        	   buf.append("').\n");
        	  
           }/*else if(extension.equals("ode")){
        	   s = "load_ode('"+ file.getPath() +"').\n";  
           }*/else if(extension.contains("xml")){
        	   buf.append("add_sbml('");
        	   buf.append(file.getPath());
        	   buf.append("').\n");
        	        
           }else if(extension.contains("ode")){
        	   buf.append("add_ode('");
        	   buf.append(file.getPath());
        	   buf.append("').\n");        	  
           }
           else{
        	   JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You can add to model only Biocham ( bc ), SBML ( xml ) or ODE ( ode ) files.");
        	   ok=false;
           }
           if(ok){
        	   
			   addedFiles.add(file);
			   StringBuffer bf=new StringBuffer();
			   if(modelFile!=null){
				   bf.append(modelFile.getName());	   
				   
			   }
			   for(int i=1;i<addedFiles.size();i++){
				   bf.append(" , ");
				   bf.append(addedFiles.get(i).getName());
			   }
			   setModelToolTip(bf.toString());
			   bf=null;
			  	
			   ((ParamTableMolecules)getMolecules().getParamTable()).clearCheckMolecules(((ParamTableMolecules)getMolecules().getParamTable()).getCheckMolecules());
			   ((ParamTableParameters)getParameters().getParamTable()).resetSavedResponses();
			   ((ParamTableInitConc)getInitConditions().getParamTable()).resetSavedResponses();
			   ((ParamTableVolumes)getVolumes().getParamTable()).resetSavedResponses();
			   ((ParamTableEvents)getEvents().getParamTable()).resetSavedResponses();
			   ((ParamTableMacros)getMacros().getParamTable()).resetSavedResponses();
			   ((ParamTableDeclarations)getDeclarations().getParamTable()).resetSavedResponses();
			   ((ParamTableRules)getRules().getParamTable()).resetSavedResponses();
			   ((ParamTableCTLSpecifications)getCtlSpecifications().getParamTable()).resetSavedResponses();
			   ((ParamTableLTLSpecifications)getLtlSpecifications().getParamTable()).resetSavedResponses();
        	   buf.append("list_molecules.\n");
        	   buf.append("check_molecules.\n");    		   
    		   sendToBiocham(buf.toString());
    		   
           }
           buf=null;
           filename=null;
           extension=null;
	   }
   }
  
   /**
    * Sends a command to Biocham to execute numerical simulation over the model with the specified options and parameters
    * 
    * @param precommand String which is composition of atomic commands that concern the parameters 
    * and options for the numerical simulation that is going to be executed.
    * 
    */
   public void executeNumSimulation(String command) {
	 
	   model.clearPlotData();
	   if(!isEnableExport()){
		   JOptionPane.showMessageDialog(BiochamMainFrame.frame, "First you have to save this model.");
		   
	   }else{
		   setWhoPopupSimulation(null);
		   sendToBiocham(command);
			
	   }
	}
   
   
   /**
    * Executes the command export_xxxx to Biocham, which exports the model to one of the possible model's formats (sbml, dot, ode, bc, nusmv, latex, prolog, lotos).
    */
   public void exportModelAs(String s) {
		
	  		
		StringBuffer buf=new StringBuffer();
		if(!isEnableExport()){
			JOptionPane.showMessageDialog(BiochamMainFrame.frame, "First you have to save this model.");
		}else{
		
			//createModelBackup();
			File file=null;						
			Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    		Utils.fileChooser.setFileFilter(null);	
			String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,"exportBiocham");			
			if (rep!=null) {
				file=new File(rep);
				
				Utils.debugMsg(">>>> " + file.getAbsolutePath());
				String pathfile=file.getAbsolutePath();
				
				if(!pathfile.contains(s) && !s.contains("sbml") && !s.contains("prolog")){
					pathfile+="."+s;				
				}else if(s.contains("sbml")){
					if(!pathfile.contains("xml")){
						pathfile+=".xml";
					}
				}else if(s.contains("prolog")){
					if(!pathfile.contains("pl")){
						pathfile+=".pl";
					}
				}
				
				Utils.debugMsg(">>>> " + pathfile);
				if(s.contains("dot")){
					DialogChooseOptions options=new DialogChooseOptions(BiochamMainFrame.frame);
					options.createDotOptionsDialog();
					String listOfOptions=options.getOptions();
					if(listOfOptions.equals("[]")){
						buf.append("export_dot('");
						buf.append(pathfile);
						buf.append("').\n");
					}else{
						buf.append("export_dot('");
						buf.append(pathfile);
						buf.append("',");
						buf.append(listOfOptions);
						buf.append(").\n");
						//cmd="export_dot('"+pathfile+"',"+listOfOptions+").\n";
					}
					biochamInput.print(buf.toString());
					
				}else if(s.contains("ode")){
					buf.append("export_ode('");
					buf.append(pathfile);
					buf.append("').\n");
					
				}else if(s.contains("bc")){
					
					buf.append("export_biocham('");
					buf.append(pathfile);
					buf.append("').\n");
					biochamInput.print(buf.toString());
				}else if(s.contains("latex")){
					buf.append("export_ode_latex('");
					buf.append(pathfile);
					buf.append("').\n");					
					biochamInput.print(buf.toString());
				}else if(s.contains("nusmv")){				
					buf.append("export_nusmv('");
					buf.append(pathfile);
					buf.append("').\n");
					biochamInput.print(buf.toString());
				}else if(s.contains("sbml")){	
					buf.append("export_sbml('");
					buf.append(pathfile);
					buf.append("').\n");					
					biochamInput.print(buf.toString());
				}else if(s.contains("lotos")){
					buf.append("export_lotos('");
					buf.append(pathfile);
					buf.append("').\n");
					biochamInput.print(buf.toString());
				}else if(s.contains("prolog")){		
					buf.append("export_prolog('");
					buf.append(pathfile);
					buf.append("').\n");
					biochamInput.print(buf.toString());
				}else if(s.contains("slp")){	
					buf.append("export_slp('");
					buf.append(pathfile);
					buf.append("').\n");
					biochamInput.print(buf.toString());
				}else{
					Utils.debugMsg("Selection Problem.");
				}
				buf.delete(0,buf.length()-1);	
				pathfile=null;
			}
			file=null;
			rep=null;
			
		}
		buf=null;
	}
	
   /**
    * Returns the model element which name is given as an argument.
    * 
    * @param name The model's element's name
    */
   public synchronized BiochamModelElement getModelElement(String name){
		   
	   
		   if(name.equals("Parameters")){
			   return getParameters();
		   }
		   else if(name.contains("Initial")){
			   return getInitConditions();
		   }
		   else if(name.equals("Reactions")){
			   return getRules();
		   }
		   else if(name.equals("Boolean Temporal Properties")){
			   return getCtlSpecifications();
		   }else if(name.equals("Numerical Temporal Properties")){
			   return getLtlSpecifications();
		   }
		   else if(name.equals("Declarations")){
			   return getDeclarations();
		   }
		   else if(name.equals("Events")){
			   return getEvents();
		   }
		   else if(name.equals("Macros")){
			   return getMacros();
		   }
		   else if(name.equals("Molecules")){
			   return getMolecules();
		   }
		   else if(name.equals("Volumes")){
			   return getVolumes();
		   }
		   else if(name.equals("Conservation Laws")){
			   return getConservationLaws();
		   }
		   else return null;
   }
	   
	
   /**
    * Parses the input trace file and plots the trace of the the old numerical simulation.
    * 
    * @param f1 File with extension csv,xls or plot, and which content represents trace data. 
    * This data is represented as comma separated values, in a textual(separation with spaces) or Excel format, 
    * or in a plot file which contains a reference to the file which contains the necessary data.
    */
   public void plotTraceData(File f1) {
		
		BiochamPlot bPlot=new BiochamPlot(this,"Trace");
		bPlot.setName("Trace");
		boolean ok=bPlot.drawPlot(f1.getAbsolutePath());
		bPlot.getDrawingArea().getPlot().fillPlot();
	
		if(ok){
	        UniversalScrollPane jsp=new UniversalScrollPane(bPlot);
	        jsp.setBackground(Color.WHITE);
	        model.getSimulationsPanel().tabbedPane.add("Trace",jsp);
	        model.getSimulationsPanel().tabbedPane.setSelectedIndex(model.getSimulationsPanel().tabbedPane.getTabCount()-1);
	        jsp=null;
		}
		bPlot=null;
	}
	
   /**
    *  Stops Biocham execution and restarts the model to its last saved state. 
    *  From time to time, the model's state is being saved to a temporary backup file, mainly in a time before executing some known demanding
    *  functions from Biocham environment.
    */
   public void restartBiochamProcess() {
	   	   
		
		if(getModelFileFromBackup()!=null){
			
			setPauseLoggerThread(true);
			loggerThread.setEof(true);		
			biochamProcess.destroy();			
			try {
				biochamInput.close();
				biochamOutput.close();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			biochamInput=null;
			biochamOutput=null;			
			connectToBiocham(new File(getModelFileFromBackup()),true);
			int cnt=0;		
			
			String readLine="";
			try {		
				while ((readLine = biochamOutput.readLine()) !=null) {
				
					cnt++;
					if(cnt>5 && readLine.contains("biocham:")){
						break;
					}
				}		
			} catch (IOException e) {		
				// TODO Auto-generated catch block
				e.printStackTrace();
			}				
			loggerThread=null;
			loggerThread = new BiochamLoggerThread(this);
			loggerThread.setName(getModelName());
			loggerThread.start();			
			setPauseLoggerThread(false);
			readLine=null;
		}
	}
	
	
   
   /**
    * Checks if the model has a store file. Loading a model from an existing file, 
    * has enableExport true, because its store file already exists. Opening an empty model
    * doesn't have a store file until it has been saved for its first time.
    * 
    * This is used for managing the availability of model's functions because some of them are not 
    * possible if the model hasn't been saved yet. 
    * It is also used for managing different views for the Workbench toolbar.
    *  
    */
   public boolean isEnableExport() {
		return enableExport;
   }
	
   /**
    * Sets a value to enableExport
    */
   public void setEnableExport(boolean enableExport) {
	   this.enableExport = enableExport;
   }
   
   /**
    * Returns an instance of the current model.
    */
   public BiochamModel getModel() {
	   return model;
   }
	
   /**
    * Sets a reference to the instance of the current model.
    */
   public void setModel(BiochamModel model) {
	   this.model = model;
   }
	
   /**
    * Returns the communication channel from the Graphical Interface to Biocham. 
    */
   public PrintStream getBiochamInput() {
	    return biochamInput;
   }
	/**
	 * Sets a reference to the communication channel from the GUI to Biocham.
	 */
	public void setBiochamInput(PrintStream biochamInput) {
		this.biochamInput = biochamInput;
	}
	
	/**
	 * Returns the communication channel from Biocham to the Graphical Interface.
	 */
	public BufferedReader getBiochamOutput() {		
		return biochamOutput;
	}
	
	/**
	 * Sets a reference to the communication channel from Biocham to the Graphical Interface.
	 */
	public void setBiochamOutput(BufferedReader biochamOutput) {
		this.biochamOutput = biochamOutput;
	}	
	
	/**
	 * Sets a tooltip to the model. Its value is the model's name. 
	 */
	private void setModelToolTip(String name) {
		this.modelToolTip=name;			
	}
	
	/**
	 * Gets the declarations model's element
	 */
	public BiochamModelElement getDeclarations() {
		return declarations;
	}
	
	/**
	 * Sets the declarations model's element
	 */
	public void setDeclarations(BiochamModelElement declarations) {
		this.declarations = declarations;
	}
	
	/**
	 * Gets the events model's element
	 */
	public BiochamModelElement getEvents() {
		return events;
	}
	
	/**
	 * Sets the events model's element
	 */	
	public void setEvents(BiochamModelElement events) {
		this.events = events;
	}
	
	/**
	 * Gets the Initital Concentrations model's element
	 */
	public BiochamModelElement getInitConditions() {
		return initConditions;
	}
	
	/**
	 * Sets the Initial Concentrations model's element
	 */
	public void setInitConditions(BiochamModelElement initConditions) {
		this.initConditions = initConditions;
	}
	
	/**
	 * Gets the model's name from its file's name
	 */
	public String getModelName() {
		return modelName;
		/*if(model!=null){
			if(model.isSaved()){
				int ind=modelName.indexOf(".");
				if(ind>=0){
					return modelName.substring(0, ind);
				}else 
					return modelName;
			}else{
				if(modelName!=null){
					return modelName;
				}else 
					return null;
			}
		}else 
			return null;*/
	}
	
	/**
	 * Sets model's name
	 */
	public void setModelName(String modelName) {
		this.modelName = modelName;
		if(modelFile==null){
			if(modelName.endsWith(".bc")){
				modelName=modelName.substring(0,modelName.length()-3);
			}
			modelFile=new File(modelName.replaceAll(" ","")+".bc");	
		}		
		this.modelFileFromBackup=modelName.replaceAll(" ","")+"_Backup"+".bc";
		setModelFileFromBackup(this.modelFileFromBackup);
		Utils.debugMsg(modelFileFromBackup);
	}
	
	/**
	 * Gets the parameters model's element
	 */
	public BiochamModelElement getParameters() {
		return this.parameters;
	}
	
	/**
	 * Sets the parameters model's element
	 */
	public void setParameters(BiochamModelElement parameters) {
		this.parameters = parameters;
	}
	
	/**
	 * Gets the rules model's element
	 */
	public BiochamModelElement getRules() {
		return rules;
	}
	
	/**
	 * Sets the rules model's element
	 */
	public void setRules(BiochamModelElement rules) {
		this.rules = rules;
	}
	
	/**
	 * Gets the CTL Specifications model's element
	 */
	public BiochamModelElement getCtlSpecifications() {
		return ctlSpecifications;
	}
	
	/**
	 * Sets the CTL Specifications model's element
	 */
	public void setCtlSpecifications(BiochamModelElement specifications) {
		this.ctlSpecifications = specifications;
	}
	
	/**
	 * Gets the list of all added files to the model
	 */
	public ArrayList<File> getAddedFiles() {
		return this.addedFiles;
	}
	
	/**
	 * Gets the model's index
	 */
	public int getIndex() {
		return index;
	}
	
	/**
	 * Sets the model's index
	 */
	public void setIndex(int index) {
		BiochamModel.index = index;
	}
	
	/**
	 * Adds a reference of the file which data was added to the model.
	 */
	public void addAddedFile(File fileName){
		addedFiles.add(fileName);
	}
	
	/**
	 * Gets model's tooltip
	 */
	public String getModelToolTip() {
		return modelToolTip;
	} 
			
	/**
	 * Gets a reference to the Biocham-GUI Communication thread
	 */
	public BiochamLoggerThread getLoggerThread() {
		return loggerThread;
	}
	
	/**
	 * Sets a reference to the Biocham-GUI Communication thread
	 */
	public void setLoggerThread(BiochamLoggerThread loggerThread) {
		this.loggerThread = loggerThread;
	}
   
	/**
	 * Returns the model's name as its basic description.
	 */
	public String toString() {		  
	      return getModelName();
	}
	
	/**
	 * Tries to connect to the user's local printer to print the model's state file.
	 */
	public void print() {		
		// TO BE DONE!
	}
	
	/**
	 * Sets a reference to the model's plot file 
	 */
	public void setPlotFile(File file) {
		this.plotFile=file;		
	}
	
	/**
	 * Gets the reference of the model's plot file 
	 */
	public File getPlotFile() {
		return this.plotFile;		
	}
	
	/**
	 * Gets the macros model's element
	 */
	public BiochamModelElement getMacros() {
		return macros;
	}
	

	/**
	 * Sets the macros model's element
	 */
	public void setMacros(BiochamModelElement macros) {
		this.macros = macros;
	}
	

	/**
	 * Gets the molecules model's element
	 */
	public BiochamModelElement getMolecules() {
		return molecules;
	}

	/**
	 * Sets the molecules model's element
	 */
	public void setMolecules(BiochamModelElement molecules) {
		this.molecules = molecules;
	}
	

	/**
	 * Gets the LTL Specifications model's element
	 */
	public BiochamModelElement getLtlSpecifications() {
		return ltlSpecifications;
	}
	
	/**
	 * Sets the LTL Specifications model's element
	 */	
	public void setLtlSpecifications(BiochamModelElement ltlSpecifications) {
		this.ltlSpecifications = ltlSpecifications;
	}
	
	/**
	 * Gets the volumes model's element
	 */
	public BiochamModelElement getVolumes() {
		return volumes;
	}
	
	/**
	 * Sets the volumes model's element
	 */
	public void setVolumes(BiochamModelElement volumes) {
		this.volumes = volumes;
	}	

	/**
	 * Sets to a model's index of its creation
	 */
	public void setModelNodeIndex(int nodeIndex) {
		this.modelNodeIndex=nodeIndex;
		
	}
	
	/**
	 * Gets the model's index of its creation
	 */
	public int getModelNodeIndex() {
		return modelNodeIndex;
	}
		
	/**
	 * Closes the model's communication with Biocham, disposes its communication thread, closes the communications streams,
	 * sets all the references to null so the garbage collection can do its job. 
	 */
	public void disposeBiochamModel()  {
		
		modelBackupFile.deleteOnExit();
		biochamProcess.destroy();
		graphEditor.dispose();
		tabTitle=null;
		graphEditor=null;
		loggerThread.disposeFields();		
		loggerThread=null;
		arguments=null;
		biochamInput.close();//input for the BioCham process...
		biochamInput=null;
		try{
			biochamOutput.close();//response of the Biocham process...
		}catch(Exception e){}
		modelName=null;
		parameters.disposeElements();
		parameters=null;
		initConditions.disposeElements();
		initConditions=null;
		rules.disposeElements();
		rules=null;
		ctlSpecifications.disposeElements();
		ctlSpecifications=null;
		ltlSpecifications.disposeElements();
		ltlSpecifications=null;
		declarations.disposeElements();
		declarations=null;
		conservationLaws.disposeElements();
		conservationLaws=null;
		events.disposeElements();
		events=null;
		macros.disposeElements();
		macros=null;
		molecules.disposeElements();
		molecules=null;
		volumes.disposeElements();
		volumes=null;		
		
		modelFile=null;
		addedFiles.clear();
		addedFiles=null;

		modelToolTip=null;		
		modelElements=null;
		
		biochamOutput=null;
		biochamExecutable=null;;
		model=null;;
		plotFile=null;
		//model_plots.clear();
		//model_plots=null;
		modelNodeIndex=0;
		plotData.clear();
		plotData=null;
		//experimentalPlots.clear();
		//experimentalPlots=null;
		model=null;
		modelWarnings.disposeElement();
	//	commandLine.disposeElement();
		rGraph=null;
		iGraph=null;
		odePlots.clear();
		odePlots=null;
		booleanPlots.clear();
		booleanPlots=null;
		
	}
	
	
	
	/**
	 * Gets the connection number
	 */
	public int getConnectionNumber() {
		return connectionNumber;
	}
	
	/**
	 * Sets the connection number
	 */
	public void setConnectionNumber(int number) {
		this.connectionNumber = number;
	}
	
	/**
	 * Sets a reference to the model's generated Influence Graph file
	 */
	public void setInfluencesImageFile(File imageFile) {
		influencesImageFile=imageFile;		
	}
	
	/**
	 * Sets a reference to the model's generated Reaction Graph file
	 */
	public void setReactionImageFile(File imageFile) {
		reactionImageFile=imageFile;		
	}
	
	/**
	 * Gets the model's generated Influence Graph file
	 */
	public File getInfluencesImageFile() {		
		return influencesImageFile;
	}
	
	/**
	 * Gets the model's generated Reaction Graph file
	 */
	public File getReactionImageFile() {		
		return reactionImageFile;
	}
	
	/**
	 * Gets the model's generated Reaction Graph
	 */
	public UniversalScrollPane getReactionGraph() {
		return reactionGraph;
	}
	
	/**
	 * Sets a reference to the model's generated Reaction Graph
	 */
	public void setReactionGraph(UniversalScrollPane reactionGraph) {
		this.reactionGraph = reactionGraph;
	}
	
	/**
	 * Gets the model's generated Influences Graph
	 */
	public UniversalScrollPane getInfluencesGraph() {
		return influencesGraph;
	}
	
	/**
	 * Sets a reference to the model's generated Influences Graph
	 */
	public void setInfluencesGraph(UniversalScrollPane influencesGraph) {
		this.influencesGraph = influencesGraph;
	}
	
	/**
	 * Gets the conservation laws model's element
	 */
	public BiochamModelElement getConservationLaws() {
		return conservationLaws;
	}
	
	/**
	 * Sets a reference to the conservation laws model's element
	 */
	public void setConservationLaws(BiochamModelElement conservationLaws) {
		this.conservationLaws = conservationLaws;
	}
	
	/**
	 * Gets the parent node of the BochamWarnings node in the tree explorer
	 */
	public DefaultMutableTreeNode getWarningsParent() {
		return warningsParent;
	}
	
	/**
	 * Sets a reference to the parent node of the BochamWarnings node in the tree explorer
	 */
	public void setWarningsParent(DefaultMutableTreeNode warningsParent) {
		this.warningsParent = warningsParent;
	}
	
	/**
	 * Gets the warnings model's element
	 */
	public BiochamWarnings getModelWarnings() {
		return modelWarnings;
	}
	
	/**
	 * Sets a reference to the warnings model's element
	 */
	public void setModelWarnings(BiochamWarnings modelWarnings) {
		this.modelWarnings = modelWarnings;
	}
	
	/**
	 * Gets the Biocham CommandPrompt
	 */
	public SimpleCommandLine getCommandLine() {
		return commandLine;
	}
	
	/**
	 * Sets a reference to the Biocham CommandPrompt
	 */
	public void setCommandLine(SimpleCommandLine commandLine) {
		this.commandLine = commandLine;
	}
	
	/**
	 * Returns the model's backup filepath
	 */
	public String getModelFileFromBackup() {
		return modelFileFromBackup;
		
	}
	/**
	 * Sets the name of the model's backup filepath
	 */
	public void setModelFileFromBackup(String modelFileFromBackup) {		
		if(!modelFileFromBackup.endsWith(".bc")){
			modelFileFromBackup+=".bc";
		}	
		if(modelBackupFile==null){
			modelBackupFile=new File(modelFileFromBackup);
			Utils.debugMsg(modelBackupFile.getAbsolutePath());
		}
		this.modelFileFromBackup = modelFileFromBackup;
	}
	
	/**
	 * Checks if the communication thread of the model is paused
	 */
	public boolean isPauseLoggerThread() {
		return pauseLoggerThread;
	}
	
	/**
	 * Turns off(false) or on(true) a pause to the communication thread of the model with Biocham environment
	 */
	public void setPauseLoggerThread(boolean pauseLoggerThread) {
		this.pauseLoggerThread = pauseLoggerThread;
	}
	
	/**
	 * Checks if the workbench is in its commandline mode
	 */
	public boolean isCommandLineMode() {
		return true;
	}
	
	/**
	 * Turns off(false) or on(true) the commandline working mode of the workbench
	 */
	public void setCommandLineMode(boolean commandLineMode) {
		this.commandLineMode = commandLineMode;		
	}
	
	/**
	 * Gets the class type of the influence graph
	 */
	public CustomZoomableImagePanel getIGraph() {
		return iGraph;
	}
	
	/**
	 * Sets a reference to the class where is the influence graph being created
	 */
	public void setIGraph(CustomZoomableImagePanel graph) {
		iGraph = graph;
	}
	
	/**
	 * Gets the class type of the reaction graph
	 */
	public CustomZoomableImagePanel getRGraph() {
		return rGraph;
	}
	
	/**
	 * Sets a reference to the class where is the reaction graph being created
	 */
	public void setRGraph(CustomZoomableImagePanel graph) {
		rGraph = graph;
	}	
	/**
	 * Checks if the model's plot image has been already added(dragged and dropped) to the Comparison area
	 */
	public boolean isDraggedAndDropped() {
		return draggedAndDropped;
	}

	/**
	 * Sets the variable that tells if the model's plot image has been added(dragged and dropped) to the Comparison area
	 * @param draggedAndDropped True, if it has been added. False, if it hasn't been added yet.
	 */
	public void setDraggedAndDropped(boolean draggedAndDropped) {
		this.draggedAndDropped = draggedAndDropped;
	}   
	
	/**
	 * Sets a reference to the plot's data. Plot's data consists of pairs of molecule's name and its demanded value (min or max).
	 */
	public void setPlotData(HashMap<String, String> pd) {
		plotData=pd;		
	}
	
	/**
	 * Gets the plot's data
	 */
	public HashMap<String, String> getPlotData() {
		return plotData;
	}
	
	/**
	 * Clears the previous plot's data
	 */
	public void clearPlotData(){
		plotData.clear();
	}			
	
	/**
	 * Checks if the model has been saved.
	 */
	public boolean isSaved() {
		return saved;
	}
	
	/**
	 * Sets the control variable which says if the model has been saved yet or no.
	 * 
	 * @param saved True, if the model has already been saved. false, if the model hasn't been saved yet.
	 */
	public void setSaved(boolean saved) {		
		this.saved = saved;
		//getTreeInstance().modelMenu.refreshed(this);	
	}	
	
	/**
	 * Returns the model's file
	 */
	public File getModelFile() {
		return modelFile;
	}

	/**
	 * Sets a reference to the model's file 
	 */
	public void setModelFile(File modelFile) {
		this.modelFile = modelFile;
	}

	public void addPlotObject(PlotImage plotImg) {
		plotImages.add(plotImg);
	}

	public ArrayList<PlotImage> getPlotImages() {
		return plotImages;
	}


	public String getWho() {
		return who;
	}

	public void setWho(String who) {
		this.who = who;
	}


	public BiochamDynamicTree getTreeInstance() {
		return treeInstance;
	}


	public void setTreeInstance(BiochamDynamicTree treeInstance) {
		this.treeInstance = treeInstance;
	}

	public BiochamGraphEditorDesktop getGraphEditor() {
		
		return graphEditor;
	}

	public void setGraphEditor(BiochamGraphEditorDesktop graphEditor) {
		this.graphEditor = graphEditor;
		setReactionsGraphEditor(graphEditor.getGraph());
	}


	public BiochamGraph getReactionsGraphEditor() {
		return reactionsGraphEditor;
	}


	public void setReactionsGraphEditor(BiochamGraph reactionsGraph) {
		this.reactionsGraphEditor = reactionsGraph;
	}



	public BiochamPlot getCurrentPlot() {
		return currentPlot;
	}

	public void setCurrentPlot(BiochamPlot currentPlot) {
		this.currentPlot = currentPlot;
	}

	public int getMaxSizeOfKinetics(){
		
		int size=0;
		for(int i=0;i<getKinetics().size();i++){
			int s=(getKinetics().get(i)[1].toString().length()+1)*(Utils.menuBarFont.getSize()/2);
			
			if(s>size){
				size=s;
			}
		}
		
		return size;
	}
	

	public ArrayList<Object[]> getKinetics() {
		return kinetics;
	}

	public void setKinetics(ArrayList<Object[]> kinetics) {
		this.kinetics = kinetics;
	}
	

	public void setNewPlotTab(JPanel p) {
		// TODO Auto-generated method stub
		
	}

	public JPanel getNewPlotTab() {
		return newPlotTab;
	}
	public String getSimulationType() {
		return simulationType;
	}


	public void setSimulationType(String simulationType) {
		this.simulationType = simulationType;
	}

	public int getBoolCounter() {
		return boolCounter;
	}

	public void setBoolCounter(int boolCounter) {
		this.boolCounter = boolCounter;
	}

	public int getOdeCounter() {
		return odeCounter;
	}
	
	public void setIgCounter(int igCounter) {
		this.igCounter = igCounter;
	}
	public int getIgCounter() {
		return igCounter;
	}
	public void setRgCounter(int rgCounter) {
		this.rgCounter = rgCounter;
	}
	public int getRgCounter() {
		return rgCounter;
	}
	
	public int getNhCounter() {
		return nhCounter;
	}





	public void setNhCounter(int nhCounter) {
		this.nhCounter = nhCounter;
	}





	public void setOdeCounter(int odeCounter) {
		this.odeCounter = odeCounter;
	}

	public int getStochCounter() {
		return stochCounter;
	}

	public void setStochCounter(int stochCounter) {
		this.stochCounter = stochCounter;
	}

	public int getSimulationCounter() {
		
		if(simulationType.equals("ODE")){
			return this.odeCounter;
		}else if(simulationType.equals("Boolean")){
			return this.boolCounter;
		}else {
			return this.stochCounter;
		}
		
	}
	
	public int getIGraphCounter() {
		return this.igCounter;
	}
	public int getRGraphCounter() {
		return this.rgCounter;
	}
	
	public HashMap<String, BiochamPlot> getOdePlots() {
		return odePlots;
	}

	public void setOdePlots(HashMap<String, BiochamPlot> plots) {
		this.odePlots = plots;
	}

	public HashMap<String, BooleanPlot> getBooleanPlots() {
		return booleanPlots;
	}

	public void setBooleanPlots(HashMap<String, BooleanPlot> booleanPlots) {
		this.booleanPlots = booleanPlots;
	}

	public boolean isLoadingModel() {
		return loadingModel;
	}

	public void setLoadingModel(boolean loadingModel) {
		this.loadingModel = loadingModel;
	}

	public int getCounter() {
		return counter;
	}

	public void setCounter(int counter) {
		this.counter = counter;
	}

	public void setInitFileModelTab(){
		WorkbenchArea.tabbedPane.addTab(tabTitle,Icons.icons.get("SSicon.png"+0.4),fileModelViewSP);
		BiochamDynamicTree.workbench.resetDividerLocation();
	}



	public void exportKinetics() {
		String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,"txt");			
		final File file;
		if (rep!=null) {
			if(!rep.endsWith("txt")){
				rep+=".txt";
			}
			file=new File(rep);
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					StringBuffer fileContent = new StringBuffer();
					fileContent.append("Init"+"\t"+"ODE\n\n");				
					String val1,val2;
					for (int i = 0; i < model.getKinetics().size(); i++) {
					 
						val1=model.getKinetics().get(i)[0].toString();
						val2=model.getKinetics().get(i)[1].toString();
						// ... continue to read each cell in a row
						fileContent.append(val1+"\t"+val2+"\n");
						// ... continue to append each cell value
					}
					val1=null;val2=null;
					FileWriter fileWriter;
					try {
						fileWriter = new FileWriter(file);
						fileWriter.write(fileContent.toString());
						fileWriter.flush();
						fileWriter.close();
					} catch (IOException e) {
						// TODO Auto-generated catch block					
						e.printStackTrace();
						
					}
					fileWriter=null;
					fileContent=null;				
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
			sw.start();
		}/*else{
			file=new File(Utils.getNameWithoutExtension(modelFile.getName()+"_Kinetics.txt"));
		}*/
		rep=null;
		
		
	}




	public DialogKinetics getKineticsDialog() {
		return kineticsDialog;
	}

	public void setKineticsDialog(DialogKinetics kineticsDialog) {
		this.kineticsDialog = kineticsDialog;
	}
	public JPanel getWarningsPanel() {
		return warningsPanel;
	}
	public void setWarningsPanel(JPanel warningsPanel) {
		this.warningsPanel = warningsPanel;
	}
	synchronized public BiochamModelEditor getModelEditor() {
		modelEditor.setDividerLocation(modelEditor.getDividerLocation());
		return modelEditor;
	}

	public void setModelEditor(BiochamModelEditor modelEditor) {
		this.modelEditor = modelEditor;
	}
	public BiochamModelElement[] getModelElements() {
		return modelElements;
	}

	public JTextArea getInitModelFileView() {
		return initModelFileView;
	}

	public void setInitModelFileView(JTextArea initModelFileView) {
		this.initModelFileView = initModelFileView;
	}
	public boolean isExistingModel() {
		return existingModel;
	}

	public void setExistingModel(boolean existingModel) {
		this.existingModel = existingModel;
	}   
	public SimulationView getSimulationsPanel() {
		return simulationsPanel;
	}
	public void setSimulationsPanel(SimulationView simulationsPanel) {
		this.simulationsPanel = simulationsPanel;
	}

	public boolean isInitConcD() {
		return initConcD;
	}

	public void setInitConcD(boolean initConcD) {
		this.initConcD = initConcD;
	}

	public boolean isConLawsD() {
		return conLawsD;
	}

	public void setConLawsD(boolean conLawsD) {
		this.conLawsD = conLawsD;
	}

	public boolean isParamD() {
		return paramD;
	}

	public void setParamD(boolean paramD) {
		this.paramD = paramD;
	}

	public boolean isEventsD() {
		return eventsD;
	}

	public void setEventsD(boolean eventsD) {
		this.eventsD = eventsD;
	}

	public boolean isMacrosD() {
		return macrosD;
	}

	public void setMacrosD(boolean macrosD) {
		this.macrosD = macrosD;
	}

	public boolean isDeclD() {
		return declD;
	}

	public void setDeclD(boolean declD) {
		this.declD = declD;
	}

	public boolean isVolumeD() {
		return volumeD;
	}

	public void setVolumeD(boolean volumeD) {
		this.volumeD = volumeD;
	}

	public boolean isMolsD() {
		return molsD;
	}

	public void setMolsD(boolean molsD) {
		this.molsD = molsD;
	}

	public boolean isRulesD() {
		return rulesD;
	}

	public void setRulesD(boolean rulesD) {
		this.rulesD = rulesD;
	}

	public void refreshAllViews() {
		for(int i=0;i<getModelViews().size();i++){
			getModelViews().get(i).refresh();
		}		
	}

	public int getLastSimLenght() {
		return lastSimLenght;
	}

	public void setLastSimLenght(int lastSimLenght) {
		this.lastSimLenght = lastSimLenght;
	}

	public Vector<String> getHistoryVector() {
		return historyVector;
	}

	public void setHistoryVector(Vector<String> historyV) {
		this.historyVector = historyV;
	}

	public int getHistoryIndex() {
		return historyIndex;
	}

	public void setHistoryIndex(int historyIndex) {
		this.historyIndex = historyIndex;
	}
	
	public ArrayList<IView> getModelViews() {
		return modelViews;
	}

	public void setModelViews(ArrayList<IView> modelViews) {
		this.modelViews = modelViews;
	}
	public File getModelBackupFile() {
		return modelBackupFile;
	}



	public void setModelBackupFile(File modelBackupFile) {
		this.modelBackupFile = modelBackupFile;
	}


	public BiochamFrame[] getPopupFrames() {
		return popupFrames;
	}



	public void setPopupFrames(BiochamFrame[] popupFrames) {
		this.popupFrames = popupFrames;
	}



	public BiochamFrame getGraphEditorFrame() {
		return graphEditorFrame;
	}



	public void setGraphEditorFrame(BiochamFrame graphEditorFrame) {
		this.graphEditorFrame = graphEditorFrame;
	}
	
	public AbstractionModel getAbstractionModel() {
		return abstractionModel;
	}
	public void setAbstractionModel(AbstractionModel abstractionModel) {
		this.abstractionModel = abstractionModel;
	}
	
	public AbstractionView getAbstractionView() {
		
		return abstractionView;
	}
	public void setAbstractionView(AbstractionView abstractionView) {
		this.abstractionView = abstractionView;
	}





	public IView getWhoPopup() {
		if(whoPopup!=null){
			return whoPopup;
		}else{
			return this.abstractionView;
		}
		
	}

	public IView getWhoPopupSimulation() {
		if(whoPopupSimulation!=null){
			return whoPopupSimulation;
		}else{
			return this.simulationsPanel;
		}
		
	}
	public void setWhoPopupSimulation(IView whoPopup) {
		this.whoPopupSimulation = whoPopup;				
	}


	public void setWhoPopup(IView whoPopup) {
		this.whoPopup = whoPopup;
	}





	public SimulationModel getSimulationModel() {
		return simulationModel;
	}





	public void setSimulationModel(SimulationModel simulationModel) {
		this.simulationModel = simulationModel;
	}


}
