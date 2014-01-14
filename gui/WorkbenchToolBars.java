package fr.inria.contraintes.biocham;

import fr.inria.contraintes.biocham.customComponents.BiochamInfoLabel;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.plotting.PlotsComparizonWindowUtils;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Dimension;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import javax.swing.AbstractAction;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.tree.DefaultMutableTreeNode;



/**
 * Class that builds the double and dynamic toolbar of the workbench area.
 * 
 * @author Dragana Jovanovska  
 */ 

public class WorkbenchToolBars extends MouseAdapter{
	
	
	
	JToolBar firstToolBar,secondToolbar;
	JLabel[] openedModelLabels, savedModelLabes, introLabels;//, plotOrientedLabels;
	ArrayList<JLabel> separatorsA, separatorsB;
	JLabel openModel,newModel,exit;// beggining
	JLabel addToModel,saveAsModel,exportModelAs,closeModel,previewTraceData,reactionGraphEditor;//opened model-not saved
	JLabel reactionGraph,abstractModelAs, comparisonWindow, restartBc;//saved model
	JLabel addExperimentalData,zoomIn,zoomOut;//numerical simulation already executed (there exists plot image of the model)
	BiochamDynamicTree tree;
	boolean intro=true, opened=false, saved=false, plotOnPreview=false; // the 4 states of the toolbar
	public static BiochamInfoLabel infoLabel;
	public static boolean alreadySetSeparator=false;
	
	public WorkbenchToolBars(BiochamDynamicTree t){
		
		
		tree=t;				
		firstToolBar=new JToolBar("toolbar icons");
		addSeparator(firstToolBar);		
		
		firstToolBar.setName("toolBarIcons");
		firstToolBar.setBackground(Utils.backgroundColor);	       
		firstToolBar.setFloatable(true);
		firstToolBar.setMargin(new Insets(15,10,15,10));
		firstToolBar.setRollover(true);	    
	    introLabels=new JLabel[2];
	    openedModelLabels=new JLabel[5];
	    savedModelLabes=new JLabel[6];
	   // plotOrientedLabels=new JLabel[3];	
	    separatorsA=new ArrayList<JLabel>();
	    separatorsB=new ArrayList<JLabel>();
	}
	
	
	
	public JToolBar getFirstToolBar(){
		
		// INTRO:    open,new..
		// OPENED:   open,new,addTo,saveAs,refresh,close, restartBiocham, graphEditor  //modelChecking
		// SAVED:    open,new,addTo,save,saveAs,exportTo,print,close,refresh  //modelChecking,previewTraceData,NumSimul,rGraph,modelAbstr
		// PLOTTED:  open,new,addTo,save,saveAs,exportTo,print,close,refresh  //modelChecking,previewTraceData,NumSimul,rGraph,modelAbstr,addExpData,zoomIn,zoomOut
		
		openModel=new JLabel(Icons.icons.get("folderblue.png"));    
		openModel.setToolTipText("Open an existing Biocham model");	   
		openModel.setName("openBCmodel");
		openModel.addMouseListener(this);
		introLabels[0]=openModel;
		firstToolBar.add(openModel);
		addSeparator(firstToolBar);
		addSeparator(firstToolBar);
		
		newModel=new JLabel(Icons.icons.get("new_file.png"+0.4));    
		newModel.setToolTipText("Open an empty Biocham model");	 
		newModel.setName("newBCmodel");
		newModel.addMouseListener(this);
		firstToolBar.add(newModel);
		introLabels[1]=newModel;	
		addSeparator(firstToolBar);
		addSeparator(firstToolBar);
		
		
		addToModel=new JLabel(Icons.icons.get("new_file03.png"+0.4));    
		addToModel.setToolTipText("Add to model");
		addToModel.setName("addToModel");
		addToModel.addMouseListener(this);
		addToModel.setVisible(false);
		openedModelLabels[0]=addToModel;
		firstToolBar.add(addToModel);
		addSeparator(firstToolBar);
		addSeparator(firstToolBar);
		
		
		
		exportModelAs=new JLabel(Icons.icons.get("export_file.png"+0.4));    
		exportModelAs.setToolTipText("Export Model as");	
		exportModelAs.setName("exportModelAs");
		exportModelAs.addMouseListener(this);
		firstToolBar.add(exportModelAs);
		savedModelLabes[0]=exportModelAs;
		exportModelAs.setVisible(false);
		separatorsA.add(addSeparator(firstToolBar));
		separatorsA.add(addSeparator(firstToolBar));		

		/*saveModel=new JLabel(Icons.icons.get("45.png"+1.5));    
		saveModel.setToolTipText("Save model");	
		saveModel.setName("saveModel");
		saveModel.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_S,InputEvent.CTRL_MASK),"saveModel");
		saveModel.getActionMap().put("saveModel",new AbstractAction(){

			public void actionPerformed(ActionEvent e) {
				
				BiochamDynamicTree.currentModel.saveToFile(false);
				
				
			}});

		
		
		
		saveModel.addMouseListener(this);
		saveModel.setVisible(false);		
		firstToolBar.add(saveModel);
		openedModelLabels[1]=saveModel;
		addSeparator(firstToolBar);
		addSeparator(firstToolBar);*/
		
		saveAsModel=new JLabel(Icons.icons.get("46.png"+1.5));  
		saveAsModel.setToolTipText("Save model as");	   
		saveAsModel.addMouseListener(this);
		saveAsModel.setName("saveModelAs");
		saveAsModel.setVisible(false);
		firstToolBar.add(saveAsModel);
		savedModelLabes[1]=saveAsModel;
		separatorsA.add(addSeparator(firstToolBar));
		separatorsA.add(addSeparator(firstToolBar));
		
	/*	printModel=new JLabel(Icons.icons.get("print.png"));    
		printModel.setToolTipText("Print the model");
		printModel.setName("printModel");
		printModel.addMouseListener(this);
		printModel.setVisible(false);
		firstToolBar.add(printModel);
		savedModelLabes[2]=printModel;
		separatorsA.add(addSeparator(firstToolBar));
		separatorsA.add(addSeparator(firstToolBar));*/
		
		closeModel=new JLabel(Icons.icons.get("fileclose-32.png"+0.8));    
		closeModel.setToolTipText("Close the model");	   
		closeModel.addMouseListener(this);
		closeModel.setName("closeModel");
		closeModel.setVisible(false);
		firstToolBar.add(closeModel);
		openedModelLabels[1]=closeModel;
		addSeparator(firstToolBar);
		//addSeparator(firstToolBar);
		
		/*refreshScreen=new JLabel(Icons.icons.get("Refresh3.png"+0.8));    
		refreshScreen.setToolTipText("Refresh Screen");	   
		refreshScreen.setName("refreshScreen");
		refreshScreen.addMouseListener(this);
		refreshScreen.setVisible(false);
		openedModelLabels[2]=refreshScreen;
		firstToolBar.add(refreshScreen);
		addSeparator(firstToolBar);
		addSeparator(firstToolBar);*/
		
		for(int i=0;i<separatorsA.size();i++){				
			int ind=firstToolBar.getComponentIndex(separatorsA.get(i));
			((JLabel)firstToolBar.getComponent(ind)).setText("");
		}
		
		
		return firstToolBar;
	}
	
	
	
	
	public JToolBar getSecondToolBar(){
		
		
		
		// MODEL ORIENTED:	
		if(!alreadySetSeparator){	
			secondToolbar=new JToolBar();
			secondToolbar.setBackground(Utils.backgroundColor);	       
			secondToolbar.setFloatable(true);
			secondToolbar.setMargin(new Insets(15,10,15,10));
			secondToolbar.setRollover(true);
			addSeparator(secondToolbar);
			
			
			
			
			
			//modelChecking
			/*modelChecking=new JLabel(Icons.icons.get("order.png"+0.5));    
			modelChecking.setToolTipText("Model checking...");	
			modelChecking.setName("modelChecking");
			modelChecking.addMouseListener(this);
			modelChecking.setVisible(false);
			secondToolbar.add(modelChecking);
			openedModelLabels[3]=modelChecking;
			
			
			addSeparator(secondToolbar);
			addSeparator(secondToolbar);*/
			

			//previewTraceData
			previewTraceData=new JLabel(Icons.icons.get("graph.png"+0.3));    
			previewTraceData.setToolTipText("Plot trace data");	   
			previewTraceData.addMouseListener(this);
			previewTraceData.setName("previewTraceData");
			previewTraceData.setVisible(false);
			secondToolbar.add(previewTraceData);
			openedModelLabels[2]=previewTraceData;
			addSeparator(secondToolbar);
			addSeparator(secondToolbar);
			
		/*	stopBiochamExecution=new JLabel(Icons.icons.get("biochamStop3.png"+0.7));
			stopBiochamExecution.setToolTipText("Stop Execution");
			stopBiochamExecution.setName("restartBiochamProcess");
			stopBiochamExecution.addMouseListener(this);
			stopBiochamExecution.setVisible(false);
			openedModelLabels[6]=stopBiochamExecution;
			secondToolbar.add(stopBiochamExecution);
			addSeparator(secondToolbar);
			addSeparator(secondToolbar);*/
			
			//reactionGraphEditor
			reactionGraphEditor=new JLabel(Icons.icons.get("p10.png"+0.1));
			reactionGraphEditor.setToolTipText("Open Reactions Graphical Editor");
			reactionGraphEditor.setName("reactionGraphEditor");
			reactionGraphEditor.addMouseListener(this);
			reactionGraphEditor.setVisible(false);
			openedModelLabels[3]=reactionGraphEditor;
			secondToolbar.add(reactionGraphEditor);
			addSeparator(secondToolbar);
			addSeparator(secondToolbar);
			
			
			reactionGraph=new JLabel(Icons.icons.get("chart_organisation.png"+1.8));    
			reactionGraph.setToolTipText("Reaction Graph");	  
			reactionGraph.setName("reactionGraph");
			reactionGraph.addMouseListener(this);
			reactionGraph.setVisible(false);
			savedModelLabes[2]=reactionGraph;
			secondToolbar.add(reactionGraph);
			separatorsB.add(addSeparator(secondToolbar));
			separatorsB.add(addSeparator(secondToolbar));
			
			//abstractModelAs
			abstractModelAs=new JLabel(Icons.icons.get("abstractionIcon.png"+0.8));    
			abstractModelAs.setToolTipText("Abstract Model As");	   
			abstractModelAs.setName("typing");
			abstractModelAs.addMouseListener(this);
			abstractModelAs.setVisible(false);
			savedModelLabes[3]=abstractModelAs;
			secondToolbar.add(abstractModelAs);		
			separatorsB.add(addSeparator(secondToolbar));
			separatorsB.add(addSeparator(secondToolbar));
			
			//numericalSimulation
			/*numericalSimulation=new JLabel(Icons.icons.get("chart_curve.png"+1.8));    
			numericalSimulation.setToolTipText("Simulation");	  
			numericalSimulation.setName("numSim");
			numericalSimulation.addMouseListener(this);
			numericalSimulation.setVisible(false);
			savedModelLabes[4]=numericalSimulation;
			secondToolbar.add(numericalSimulation);
			separatorsB.add(addSeparator(secondToolbar));
			separatorsB.add(addSeparator(secondToolbar));*/
			
			//comparisonWindow
			comparisonWindow=new JLabel(Icons.icons.get("kompare_111_32.png"));    
			comparisonWindow.setToolTipText("Comparison Window");	  
			comparisonWindow.setName("openComparisonWindow");
			comparisonWindow.addMouseListener(this);
			comparisonWindow.setVisible(false);
			savedModelLabes[4]=comparisonWindow;
			secondToolbar.add(comparisonWindow);
			separatorsB.add(addSeparator(secondToolbar));
			separatorsB.add(addSeparator(secondToolbar));
			
			//restartBC
			restartBc=new JLabel(Icons.icons.get("biochamStop3.png"+0.7));    
			restartBc.setToolTipText("Interrupt Model Execution");	  
			restartBc.setName("restartBc");
			restartBc.addMouseListener(this);
			restartBc.setVisible(false);
			savedModelLabes[5]=restartBc;
			secondToolbar.add(restartBc);
			separatorsB.add(addSeparator(secondToolbar));
			separatorsB.add(addSeparator(secondToolbar));
			
			infoLabel=new BiochamInfoLabel("");
			infoLabel.setVisible(true);
			secondToolbar.add(infoLabel);
			openedModelLabels[4]=infoLabel;
		
			for(int i=0;i<separatorsB.size();i++){
				int ind=secondToolbar.getComponentIndex(separatorsB.get(i));
				
				if(ind!=-1){
					((JLabel)secondToolbar.getComponent(ind)).setText("");
				}
			}
		
			alreadySetSeparator=true;
		}
		
		
			return secondToolbar;
	}
	
	
	
	
	
	public void mouseClicked(MouseEvent e) {
		
		if(e.getSource() instanceof JLabel){
			JLabel l=(JLabel)e.getSource();
			String cmd=l.getName();
			if(cmd!=null){
				if(cmd.equals("openBCmodel")){
					SwingWorker sw=new SwingWorker(){

						@Override
						public Object construct() {
							tree.treeListener.openBCmodel();
							return null;
						}

						@Override
						public void finished() {
							// TODO Auto-generated method stub
							
						}};
						sw.start(); 
					
					
					
				}else if(cmd.equals("newBCmodel")){
					SwingWorker sw=new SwingWorker(){

						@Override
						public Object construct() {
							tree.treeListener.newBCmodel();
							return null;
						}

						@Override
						public void finished() {
							// TODO Auto-generated method stub
							
						}};
						sw.start(); 
					
					
					
				}else if(cmd.equals("addToModel")){
					
					SwingWorker sw=new SwingWorker(){

						@Override
						public Object construct() {
							tree.treeListener.addToModel();
							return null;
						}

						@Override
						public void finished() {
							// TODO Auto-generated method stub
							
						}};
						sw.start();
					
					
				}else if(cmd.equals("exportModelAs")){
					
					SwingWorker sw=new SwingWorker(){

						@Override
						public Object construct() {
							tree.treeListener.exportModelAs();
							return null;
						}

						@Override
						public void finished() {
							// TODO Auto-generated method stub
							
						}};
						sw.start();
					
					
				}else if(cmd.equals("saveModelAs")){
					
					SwingWorker sw=new SwingWorker(){

						@Override
						public Object construct() {
							tree.treeListener.saveModelAs();
							return null;
						}

						@Override
						public void finished() {
							// TODO Auto-generated method stub
							
						}};
						sw.start();
					
					
				}else if(cmd.equals("numSim")){
					
					SwingWorker sw=new SwingWorker(){

						@Override
						public Object construct() {
							tree.treeListener.numericalSimulation();
							return null;
						}

						@Override
						public void finished() {
							// TODO Auto-generated method stub
							
						}};
						sw.start();
					
					
					
				}else if(cmd.equals("closeModel")){
					
					tree.treeListener.closeModel();
					
				}else if(cmd.equals("reactionGraph")){
					
					SwingWorker sw=new SwingWorker(){

						@Override
						public Object construct() {
							tree.treeListener.generateReactionGraph();
							return null;
						}

						@Override
						public void finished() {
							// TODO Auto-generated method stub
							
						}};
						sw.start();
					
					
				}else if(cmd.equals("typing")){
					
					SwingWorker sw=new SwingWorker(){

						@Override
						public Object construct() {
							tree.treeListener.doTyping();
							return null;
						}

						@Override
						public void finished() {
							// TODO Auto-generated method stub
							
						}};
						sw.start();
					
					
				}else if(cmd.equals("previewTraceData")){
					
					SwingWorker sw=new SwingWorker(){

						@Override
						public Object construct() {
							tree.treeListener.previewTraceData();
							return null;
						}

						@Override
						public void finished() {
							// TODO Auto-generated method stub
							
						}};
						sw.start();
					
					
				}/*else if(cmd.equals("addExperimentalData")){
					
					SwingWorker sw=new SwingWorker(){

						@Override
						public Object construct() {
							tree.addExperimentalData();
							return null;
						}

						@Override
						public void finished() {
							// TODO Auto-generated method stub
							
						}};
						sw.start();
					
				}else if(cmd.equals("zoomIn")){
					
					tree.zoomInImage();
					
				}else if(cmd.equals("zoomOut")){
					
					tree.zoomOutImage();
					
				}*/else if(cmd.equals("refreshScreen")){
					
					//tree.refreshPanel();
					
				}else if(cmd.equals("modelChecking")){
					
					tree.treeListener.openModelChecking();
					
				}else if(cmd.equals("reactionGraphEditor")){
					
					tree.workbench.replaceTabbedPane(tree.currentModel).setVisible(true);
				}else if(cmd.equals("openComparisonWindow")){
					
					if(!BiochamDynamicTree.compWindowAlreadyOpened){
						//PlotsComparizonWindowUtils.updateComparisonWindow();	
						JFrame d=BiochamDynamicTree.comparizonFrame;
						//JScrollPane sp=new JScrollPane();
						//sp.setViewportView(BiochamDynamicTree.comparizonWindow);
						d.getContentPane().removeAll();
						d.getContentPane().add(new JScrollPane(BiochamDynamicTree.comparizonWindow));
						//BiochamDynamicTree.comparizonWindow.setPreferredSize(BiochamMainFrame.frame.getSize());
						Point pos = BiochamMainFrame.frame.getLocationOnScreen();
						d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,	pos.y+BiochamMainFrame.frame.getSize().height/2-140);
					    d.setResizable(true);
					    d.setSize(new Dimension(480, 580));	    
					    d.setLocationRelativeTo(BiochamMainFrame.frame);
					    d.setAlwaysOnTop(false);
					    d.setFocusable(true);		    
					    d.setVisible(true);	  
					    BiochamDynamicTree.compWindowAlreadyOpened=true;
					}
				}else if(cmd.equals("restartBc")){
					if(BiochamDynamicTree.currentModel!=null){
						if(JOptionPane.showConfirmDialog(BiochamMainFrame.frame,"Are you sure you want to restart the model: "+BiochamDynamicTree.currentModel.getModelName())==JOptionPane.YES_OPTION){
							BiochamDynamicTree.currentModel.restartBiochamProcess();
						}
					}else{
						JOptionPane.showMessageDialog(BiochamMainFrame.frame,"No model is being selected for the interrup.");
					}
				}
			}
			
			cmd=null;
			l=null;
		}
	}
	
	
	
	

	public boolean isIntro() {
		return intro;
	}
	public void setIntro(boolean intro) {
		this.intro = intro;		
	}
	
	
	
	

	public boolean isOpened() {
		return opened;
	}
	public void setOpened(boolean opened) {
		this.opened = opened;
	
		
		if(opened){
			for(int i=0;i<openedModelLabels.length;i++){
				/*System.out.println(openedModelLabels[i]);
				System.out.println(openedModelLabels[i].getName());*/
				if(openedModelLabels[i]!=null){
					openedModelLabels[i].setVisible(true);
				}
				
			}			
		}else{
			for(int i=0;i<openedModelLabels.length;i++){
				openedModelLabels[i].setVisible(false);
			}	
			for(int i=0;i<separatorsA.size();i++){				
				int ind=firstToolBar.getComponentIndex(separatorsA.get(i));
				if(ind!=-1){
					((JLabel)firstToolBar.getComponent(ind)).setText("");
				}
			}
			for(int i=0;i<separatorsB.size();i++){
				int ind=secondToolbar.getComponentIndex(separatorsB.get(i));
				if(ind!=-1){
					((JLabel)secondToolbar.getComponent(ind)).setText("");
				}
			}
		}
		infoLabel.setVisible(true);
		firstToolBar.validate();
		firstToolBar.repaint();
		secondToolbar.validate();
		secondToolbar.repaint();
	}

	
	
	

	public boolean isSaved() {
		return saved;
	}
	public void setSaved(boolean saved) {
		this.saved = saved;		
		if(saved){
			for(int i=0;i<savedModelLabes.length;i++){
				savedModelLabes[i].setVisible(true);				
			}			
			for(int i=0;i<separatorsA.size();i++){				
				int ind=firstToolBar.getComponentIndex(separatorsA.get(i));
				if(ind!=-1){
					((JLabel)firstToolBar.getComponent(ind)).setText("  ");
				}
			}
			for(int i=0;i<separatorsB.size();i++){
				int ind=secondToolbar.getComponentIndex(separatorsB.get(i));
				if(ind!=-1){
					((JLabel)secondToolbar.getComponent(ind)).setText("  ");
				}
			}
		}else{
			for(int i=0;i<savedModelLabes.length;i++){
				if(	savedModelLabes[i]!=null){
					savedModelLabes[i].setVisible(false);
				}
			}			
			for(int i=0;i<separatorsA.size();i++){				
				int ind=firstToolBar.getComponentIndex(separatorsA.get(i));
				if(ind!=-1){
					((JLabel)firstToolBar.getComponent(ind)).setText("");
				}
			}
			for(int i=0;i<separatorsB.size();i++){
				int ind=secondToolbar.getComponentIndex(separatorsB.get(i));
				if(ind!=-1){
					((JLabel)secondToolbar.getComponent(ind)).setText("");
				}
			}
		}
		infoLabel.setVisible(true);
		firstToolBar.validate();
		firstToolBar.repaint();
		if(secondToolbar!=null){
			secondToolbar.validate();
			secondToolbar.repaint();
		}
		
	}

	
	
	

	public boolean isPlotOnPreview() {
		return plotOnPreview;
	}
	/*public void setPlotOnPreview(boolean plotOnPreview) {
		this.plotOnPreview = plotOnPreview;
		if(plotOnPreview){
			for(int i=0;i<plotOrientedLabels.length;i++){
				plotOrientedLabels[i].setVisible(true);
			}			
		}else{
			//for(int i=0;i<plotOrientedLabels.length;i++){
				if(plotOrientedLabels[i]!=null){
					plotOrientedLabels[i].setVisible(false);
				}
			//}			
		}
		try{
			
			infoLabel.setVisible(true);
			firstToolBar.validate();
			firstToolBar.repaint();
			secondToolbar.validate();
			secondToolbar.repaint();
		
		}catch(Exception e){}
	}*/

	private JLabel addSeparator(JToolBar toolbar){
		JLabel sep=new JLabel("  ");
		
		sep.setBackground(Utils.backgroundColor);
		sep.setForeground(Utils.backgroundColor);
		toolbar.add(sep);
		return sep;
	}
	

}
