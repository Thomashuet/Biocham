package fr.inria.contraintes.biocham;

import fr.inria.contraintes.biocham.commandLine.SimpleCommandLine;
import fr.inria.contraintes.biocham.customComponents.BiochamPlot;
import fr.inria.contraintes.biocham.customComponents.CustomLayoutAKDock;
import fr.inria.contraintes.biocham.customComponents.DnDTabbedPane;
import fr.inria.contraintes.biocham.customComponents.PlotImage;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.dialogs.CustomInputDialog;
import fr.inria.contraintes.biocham.dialogs.DialogSimulation;
import fr.inria.contraintes.biocham.menus.BiochamMenuBar;
import fr.inria.contraintes.biocham.modelData.AbstractionView;
import fr.inria.contraintes.biocham.modelData.CTLView;
import fr.inria.contraintes.biocham.modelData.LTLView;
import fr.inria.contraintes.biocham.modelData.ParamTableCTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableConservationLaws;
import fr.inria.contraintes.biocham.modelData.ParamTableLTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableMolecules;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.modelData.SimulationView;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SupportedSuffixes;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Enumeration;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;





/**
 *
 * Listener for the Model's explorer tree events or the main Biocham actions, like opening a new/existing model, quitting the interface, etc.
 * 
 **/
public class BiochamDynamicTreeActionListener implements ActionListener{

	
	DefaultMutableTreeNode lastParent;
	
	
	/**
	 * Implementation of the requested actions from the main Biocham GUI menu and toolbar.
	 * 
	 */
	public void actionPerformed(ActionEvent e) {
	
	
		if (e.getActionCommand().equals("openBCmodel")) {	
			
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					BiochamMenuBar.setMenusEnabled(true);					
					BiochamDynamicTree.workbench.wtb.setOpened(true);
					BiochamDynamicTree.workbench.wtb.setSaved(true);
					BiochamDynamicTree.workbench.wtb.setIntro(true);
					openBCmodel();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
				
		}	
		else if (e.getActionCommand().equals("newBCmodel")) {	
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					BiochamMenuBar.setMenusEnabled(true);					
					BiochamDynamicTree.workbench.wtb.setOpened(true);
					BiochamDynamicTree.workbench.wtb.setSaved(true);
					BiochamDynamicTree.workbench.wtb.setIntro(true);
					newBCmodel();	
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
			
		}	
		else if (e.getActionCommand().equals("saveModel")) {	
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					saveModel();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
			
		}	
		else if (e.getActionCommand().equals("saveModelAs")) {	
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					saveModelAs();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
			
		}	
		else if (e.getActionCommand().equals("numSim")) {	
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					numericalSimulation();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
			
		}else if(e.getActionCommand().equals("ode")){
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					odeSimDialog();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
		}else if(e.getActionCommand().equals("stochastic")){
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					stochasticSimDialog();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
		}else if(e.getActionCommand().equals("bool")){
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					booleanSimDialog();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
		}
		else if (e.getActionCommand().equals("clearWarningsFile")) {
			clearWarnings();
		} 
		else if (e.getActionCommand().equals("saveWarningsFile")) {
			
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					saveWarningsToFile();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
			
		}
		else if (e.getActionCommand().equals("modelChecking")) {	
			openModelChecking();	
		}else if(e.getActionCommand().contains("Boolean Temporal")){
			if(((ParamTableCTLSpecifications)BiochamDynamicTree.currentModel.getCtlSpecifications().getParamTable()).getCtlModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.booleanTPF.getContentPane().removeAll();				
				CTLView v=new CTLView(BiochamDynamicTree.currentModel.booleanTPF,((ParamTableCTLSpecifications)BiochamDynamicTree.currentModel.getCtlSpecifications().getParamTable()).getCtlModel());				
				JFrame d=BiochamDynamicTree.currentModel.booleanTPF;
				

				d.getContentPane().add(v);					
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	 
			}
						    		
 			
		}else if(e.getActionCommand().contains("Numerical Temporal")){
				 
			if(((ParamTableLTLSpecifications)BiochamDynamicTree.currentModel.getLtlSpecifications().getParamTable()).getLtlModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.numericalTPF.getContentPane().removeAll();				
				LTLView v=new LTLView(BiochamDynamicTree.currentModel.numericalTPF,((ParamTableLTLSpecifications)BiochamDynamicTree.currentModel.getLtlSpecifications().getParamTable()).getLtlModel());				
				JFrame d=BiochamDynamicTree.currentModel.numericalTPF;
				

				d.getContentPane().add(v);					
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	 
			}
			/* BiochamDynamicTree.workbench.setRightComponent(WorkbenchArea.tabbedPane);
			 BiochamDynamicTree.showNumericalTemporalProperties();*/
		}
		else if (e.getActionCommand().equals("typing")) {	
			doTyping();	
		} 
		else if (e.getActionCommand().equals("reactionGraph")) {	
			generateReactionGraph();
		}	
		else if (e.getActionCommand().equals("closeModel")) {	
			
			closeModel();
			
		}
		else if (e.getActionCommand().equals("printModel")) {	
			printModel();	
		}
	
		else if (e.getActionCommand().equals("exportModelAs")) {
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					exportModelAs();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
			
		}	
		else if (e.getActionCommand().equals("clearAll")) {
			// Check if the user wants to save any model to file before
			// closing....
			clearExplorerTree();
			BiochamMenuBar.setMenusEnabled(false);					
			BiochamDynamicTree.workbench.wtb.setOpened(false);
			BiochamDynamicTree.workbench.wtb.setSaved(false);
			BiochamDynamicTree.workbench.wtb.setIntro(true);
			
		}	
		else if (e.getActionCommand().equals("addToModel")) {	
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					addToModel();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
			
		}
		else if(e.getActionCommand().equals("launchGraphicalEditor")){
				
			BiochamDynamicTree.workbench.replaceTabbedPane(BiochamDynamicTree.currentModel).setVisible(true);
			
		}else if(e.getActionCommand().equals("showKinetics")){
			showModelKinetics();
		}else if(e.getActionCommand().equals("exportKinetics")){
			exportModelKinetics();
		}
			
		else if (e.getActionCommand().equals("saveParameters")) {
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					saveParameters();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
			
		}else if(e.getActionCommand().equals("searchPInvars")){
			if(BiochamDynamicTree.currentModel!=null){
				CustomInputDialog d=new CustomInputDialog(BiochamDynamicTree.currentModel.conLawsF,"P-Invariant limit", "Enter a limit on the highest value\n"+
						"found in a given P-invariant (the default is 4):", true, "4", "",null);
				if(d.getInputVal()!=null && d.getInputVal().length()>0){
					if(!d.getInputVal().equals("4")){
						try{
							int k=4;
							try{
								k=Integer.parseInt(d.getInputVal());
							}catch(Exception ep){
								k=4;
							}							
							((ParamTableConservationLaws)BiochamDynamicTree.currentModel.getConservationLaws().getParamTable()).computePinvariants(k);
						}catch(NumberFormatException ex){
							JOptionPane.showMessageDialog(BiochamMainFrame.frame, "The input should be an Integer value.","Error",JOptionPane.ERROR_MESSAGE);
						}
					}else{
						((ParamTableConservationLaws)BiochamDynamicTree.currentModel.getConservationLaws().getParamTable()).computePinvariants(-1);
					}
				}
				d.dispose();
				d=null;
			}
		}else if(e.getActionCommand().equals("checkMolecules")){
			if(BiochamDynamicTree.currentModel!=null){
				((ParamTableMolecules)BiochamDynamicTree.currentModel.getMolecules().getParamTable()).checkMolecules(BiochamMainFrame.frame);
			}
		}	
		else if (e.getActionCommand().equals("exportAsParameters")) {
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					exportParametersAs();		
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
			
		}
		else if (e.getActionCommand().equals("plotExperimentalData")) {	
			previewTraceData();
		}else if(e.getActionCommand().equals("addRule")){
			if(BiochamDynamicTree.currentModel!=null){
				((ParamTableRules)BiochamDynamicTree.currentModel.getRules().getParamTable()).addParameter();
			}
		}else if(e.getActionCommand().equals("comparisonWindow")){
			if(!BiochamDynamicTree.compWindowAlreadyOpened){
				JFrame d=BiochamDynamicTree.comparizonFrame;
				d.getContentPane().add(new JScrollPane(BiochamDynamicTree.comparizonWindow));					
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	  
			    BiochamDynamicTree.compWindowAlreadyOpened=true;
			}
			
		}else if(e.getActionCommand().equals("sMerge")){
			
		}
		else if(e.getActionCommand().equals("rMerge")){
			
		}
		else if(e.getActionCommand().equals("sDelete")){
			
		}
		else if(e.getActionCommand().equals("rDelete")){
			
		}
		else if(e.getActionCommand().equals("searchReduction")){
			
		}
		else if(e.getActionCommand().equals("searchAllReductions")){
			
		}
		else if(e.getActionCommand().equals("searchMOReduction")){
	
			
		}else if(e.getActionCommand().equals("searchAllMOReduction")){
			
		}
		else if(e.getActionCommand().equals("listIG")){
			
		}
		else if(e.getActionCommand().equals("exportIG")){
			
		}
		else if(e.getActionCommand().equals("listFunctions")){
	
			
		}else if(e.getActionCommand().equals("listNG")){
			
		}
		else if(e.getActionCommand().equals("exportNG")){
			
		}		
		else{
			Utils.debugMsg(e.toString());
			JOptionPane.showMessageDialog(BiochamMainFrame.frame,
					"Select the working model first!", "Warning",
					JOptionPane.WARNING_MESSAGE);	
		}	
		
	}
	
	
	
	/**
	 * Saves the warnings (if any) of the working with Biocham to a file
	 */
	private void saveWarningsToFile() {
				
		TreePath path=WorkbenchArea.tree.tree.getSelectionPath();    		
        if(path!=null){		            
        	WorkbenchArea.tree.tree.getSelectionModel().setSelectionPath(path);
        	DefaultMutableTreeNode selectedNode=(DefaultMutableTreeNode)path.getLastPathComponent();		            	   
        	Object nodeInfo=selectedNode.getUserObject();
        	
        	if(nodeInfo instanceof BiochamWarnings){
        		
        		BiochamWarnings w=(BiochamWarnings)nodeInfo;        		
        		Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        		String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,SupportedSuffixes.TEXT_SUFFIX);			
    			if (rep!=null) {
    				File fFile=new File(rep);
        			if (fFile.exists ()) {
        				int response = JOptionPane.showConfirmDialog (null,
        						"Overwrite existing file?","Confirm Overwrite",
        						JOptionPane.OK_CANCEL_OPTION,
        						JOptionPane.QUESTION_MESSAGE);
        				if (response == JOptionPane.OK_OPTION) {
        					try {
        						
        						PrintWriter out =new PrintWriter (new BufferedWriter (new FileWriter (fFile)));
        						out.print (w.getTarea().getText());
        						out.flush ();
        						out.close ();
        						out=null;
        					}
        					catch (IOException e) {
        						e.printStackTrace();
        					} 		
        				}
        			}else{
        				try {
    						PrintWriter out =new PrintWriter (new BufferedWriter (new FileWriter (fFile)));
    						out.print (w.getTarea().getText());
    						out.flush ();
    						out.close ();
    						out=null;
    					}
    					catch (IOException e) {
    						e.printStackTrace();    						
    					} 		
        			}
        			fFile=null;
        		}
    			rep=null;
        		w=null;
        		
        	}
        	selectedNode=null;
			nodeInfo=null;
        }	
        path=null;
	}
	
	
	
	
	/**
	 * Clears the generated warnings
	 */
	private void clearWarnings() {
		
		TreePath path=WorkbenchArea.tree.tree.getSelectionPath();    		
        if(path!=null){		            
        	WorkbenchArea.tree.tree.getSelectionModel().setSelectionPath(path);
        	DefaultMutableTreeNode selectedNode=(DefaultMutableTreeNode)path.getLastPathComponent();		            	   
        	Object nodeInfo=selectedNode.getUserObject();
        	
        	if(nodeInfo instanceof BiochamWarnings){
        		BiochamWarnings w=(BiochamWarnings)nodeInfo;
        		w.getTarea().setText("");
        		w=null;
        	}
        	nodeInfo=null;
        	selectedNode=null;
        }
        path=null;
		
	}
	
	
	
	/**
	 * Previews trace data. The trace data can be from an Excel file(*.xls), plot file(*.plot), or from a CSV file(*.csv) 
	 */
	public void previewTraceData() {
		
		
		DefaultMutableTreeNode o=getSelectedNode();
		if(o!=null){			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){
				Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
				Utils.fileChooser.setFileFilter(null);
				String rep=Utils.showOpenDialog(BiochamMainFrame.frame,"trace");			
				if (rep!=null) {
					File file=new File(rep);
					if(!file.isDirectory()){
						m.plotTraceData(file);		    	            	
					}else{
						JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Select a file, not a whole directory!", "Selection Warning",JOptionPane.WARNING_MESSAGE);
					}
					file=null;
				}
				rep=null;
			}
			m=null;
		}	
		o=null;
	}
	

	/**
	 * Exports the model in one of the possible formats( sbml, lorus, nusmv, ode, etc.)
	 */
	public void exportModelAs() {
		
		// Let the USER choose from a popup LIST: DOT,ODE,LaTex,SBML,NuSMV......
		
		Object[] possibilities = { "bc", "dot", "ode", "latex", "sbml",
				"nusmv", "lotos", "prolog", "slp" };
		String s = (String) JOptionPane.showInputDialog(BiochamMainFrame.frame,
				" How do you want to export the model file as?\n",
				"Export Biocham Model As", JOptionPane.PLAIN_MESSAGE,
				Icons.icons.get("Ok-32x32.png"), possibilities, "bc");

		if ((s != null) && (s.length() > 0)) {

			DefaultMutableTreeNode o=getSelectedNode();
			if(o!=null){			
				BiochamModel m =getModelInstanceFromSelectedNode(o);			
				if(m!=null){
					m.exportModelAs(s);
				}
				m=null;
			}	
			o=null;
		}			
		s=null;
		possibilities=null;
	}

	
	
	/**
	 * Prints all the model's data
	 */
	public void printModel() {
		
		DefaultMutableTreeNode o=getSelectedNode();
		if(o!=null){			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){
				m.print();
			}
			m=null;
		}
		o=null;
		
	}

	
	
	/**
	 * Closes the model. It needs to be re-done this method.
	 */
	public void closeModel() {
		// Check if the user wants to save the model to file before closing....
				
		removeCurrentNode();
		
	}

	
	
	/**
	 * Generates reaction graph of the current model
	 */
	public void generateReactionGraph() {
		
		DefaultMutableTreeNode o=getSelectedNode();		
		if(o!=null){
			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){
				m.setWhoPopup(null);
		    	m.sendToBiocham("draw_reactions.\n","reactionGraph");    		    	    		    	
		    			    	
			}
			m=null;
		}
		o=null;
		
	}
	
	
	
	
	/** Remove all nodes except the root node. 
	 */
	public void clearExplorerTree() {
					
			((DefaultMutableTreeNode)WorkbenchArea.tree.tree.getModel().getRoot()).removeAllChildren();
	        for(int i=0;i<BiochamDynamicTree.openedModels.size();i++){
	        	if(BiochamDynamicTree.openedModels.get(i)!=null){		        	
	        		BiochamDynamicTree.openedModels.get(i).disposeBiochamModel();
	        		BiochamDynamicTree.openedModels.remove(i);					
	        	}
	        }
	        ((DefaultTreeModel)WorkbenchArea.tree.tree.getModel()).reload();	       
	        if(BiochamDynamicTree.openedModels.size()==0){
	        	BiochamDynamicTree.workbench.setMenubarState(false);
	        	BiochamDynamicTree.workbench.wtb.setOpened(false);
	        	BiochamDynamicTree.workbench.wtb.setSaved(false);
	        	BiochamDynamicTree.workbench.wtb.setIntro(true);
			}
	}
	
	
	
	/**
	 * Removes the node with the defined index from the tree explorer
	 */
	public void removeNode(int index) {
		
		DefaultTreeModel treeModel=(DefaultTreeModel)WorkbenchArea.tree.tree.getModel();		
		treeModel.removeNodeFromParent(BiochamDynamicTree.getNode(index));
		BiochamDynamicTree.openedNodes.remove(index);
		treeModel=null;
		
	}
	
	
	
	/**
	 * Gets a handle of the model depending of the currently selected node in the main explorer tree.
	 *  
	 */
	public DefaultMutableTreeNode getModelInstanceNode(BiochamModel m){
		int nmb=BiochamDynamicTree.openedModels.size();
		for(int i=0;i<nmb;i++){
			if(BiochamDynamicTree.openedModels.get(i).getModelNodeIndex()==m.getModelNodeIndex()){
				return (DefaultMutableTreeNode)BiochamDynamicTree.openedNodes.get(i);
			}
		}
		return null;
	}
	/**
	 * Gets a handle of the selected node in the main explorer tree.
	 *  
	 */
	public static DefaultMutableTreeNode getSelectedNode(){
		
		JTree tree=WorkbenchArea.tree.tree;	
		TreePath path=tree.getSelectionPath();		
		if(path!=null){	
			
			tree.getSelectionModel().setSelectionPath(path);   	
			return (DefaultMutableTreeNode)path.getLastPathComponent();
			
		}else{
			JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Select the working model first!","Warning",JOptionPane.WARNING_MESSAGE);			
			return null;
		}			
	}
	
	
	
	
	/**
	 * Gets a handle of the node separator node in the main explorer tree.
	 *  
	 */
	public static DefaultMutableTreeNode getNodeSeparatorInstance(DefaultMutableTreeNode selectedNode){
		
		if(selectedNode.getUserObject() instanceof BiochamModel){			
			return (DefaultMutableTreeNode)selectedNode.getLastChild();			
		}else if(selectedNode.getUserObject() instanceof BiochamModelElement){			
			return (DefaultMutableTreeNode)((DefaultMutableTreeNode)((DefaultMutableTreeNode)selectedNode.getParent()).getLastChild());
		}else if(selectedNode.getUserObject() instanceof BiochamWarnings || selectedNode.getUserObject() instanceof SimpleCommandLine ){		
			return (DefaultMutableTreeNode)(DefaultMutableTreeNode)((DefaultMutableTreeNode)((DefaultMutableTreeNode)selectedNode.getParent()).getParent()).getLastChild();				
		}else if(selectedNode.getUserObject() instanceof BiochamDynamicTreeSeparator){
			return selectedNode;
		}else{
			return null;
		}
	}
	
	/**
	 * Gets a model instance object from the currently selected node in the main explorer tree.
	 *  
	 */
	public static BiochamModel getModelInstanceFromSelectedNode(DefaultMutableTreeNode node){
		return BiochamDynamicTree.currentModel;//((NodeSeparator)getNodeSeparatorInstance(node).getUserObject()).getModel();
	}
	
	
	/**
	 * Refreshes the screen of the current model and current element
	 * @param panel
	 * @param comps
	 * @param biochamModel
	 * @param elementName
	 */
	
	public static boolean isPlotTab(String name){
		 if(!name.contains("Cmaes") && !name.contains("ODE-") && !name.contains("Boolean-") && !name.contains("Stochastic-") && !name.contains("Reaction Graph") && !name.contains("Influences Graph")){
			return true;
		}else{
			return false;
		}
	}
	
	
	/**	 *  Expands all the model's component nodes.
	 */
	// If expand is true, expands all nodes in the tree. 
	// Otherwise, collapses all nodes in the tree. 
	public void expandAll(boolean expand) { 
		
		TreeNode root = (TreeNode)WorkbenchArea.tree.tree.getModel().getRoot(); 	
		TreePath path=new TreePath(root);
		expandAll(path, expand); 
		root=null;
		path=null;		
	}
	
	
	/**
	 *  Expands all the model's component nodes, when the path is given.
	 */
	private void expandAll(TreePath parent, boolean expand) { 
				
		// Traverse children 
		TreeNode node = (TreeNode)parent.getLastPathComponent(); 		
		if (node.getChildCount() >= 0) { 
			TreeNode n;
			TreePath path;
			for (Enumeration e=node.children(); e.hasMoreElements(); ) { 
				n = (TreeNode)e.nextElement(); 
				path = parent.pathByAddingChild(n); 
				expandAll(path, expand); 
			}
			n=null;
			path=null;
		} 
		// Expansion or collapse must be done bottom-up 
		if (expand) { 
			try{
				WorkbenchArea.tree.tree.expandPath(parent);
			}catch(Exception exc){
				Utils.debugMsg("Exception in expandAll tree....\n"+exc.getMessage());
			}
		}
	}
	
	
	/**
	 * ????
	 * Old code??,
	 * */
	public static void addCmaesImageNode(BiochamPlot plot,BiochamModel m){
		
		String name="CmaesSearchPlot";
		//DefaultMutableTreeNode lastChild=getNodeSeparatorInstance(getModelInstanceNode(m));
		
		PlotImage expPlotImg=null;		
		boolean added=false;
		if(!added){
				expPlotImg=new PlotImage(name,m);		            	
    			m.addPlotObject(expPlotImg);
    			expPlotImg.setPlot(plot);
		}          		            		
		expPlotImg.setI(BiochamLoggerThread.i);
		added=false;
		//int numChildren=lastChild.getChildCount();		
		/*JTree tree=WorkbenchArea.tree.tree;				
		DefaultTreeModel treeModel = (DefaultTreeModel)tree.getModel();		
		if(numChildren>2){
        		           
			DefaultMutableTreeNode newNode=WorkbenchArea.tree.addNodeObject((DefaultMutableTreeNode)lastChild.getChildAt(2),expPlotImg);
			treeModel.reload();	
			expandAll(true);
			TreePath selectionPath=new TreePath(newNode.getPath());
			tree.setSelectionPath(selectionPath);    	            	
			selectionPath=null;
			newNode=null;
			newNode=null;
			selectionPath=null;
			
    	            	
		}	*/
		UniversalScrollPane jsp=new UniversalScrollPane(plot);
		jsp.setBackground(Color.WHITE);
		m.getModelEditor().getTabbedPane().add(name,jsp);
		m.getModelEditor().getTabbedPane().setSelectedIndex(m.getModelEditor().getTabbedPane().getTabCount()-1);
				
		name=null;		
		expPlotImg=null;
		jsp=null;
		
	}

	
	
	/**
	 * Exports the model's parameters in a file of one of the possible formats (bc, sbml, latex, ode, dot, lotos, etc.)
	 */
	public void exportParametersAs() {
		
		
		
		Object[] possibilities = {"bc", "dot", "ode", "latex", "sbml", "nusmv", "lotos", "prolog", "slp"};
		
		String s = (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,
					" How do you want to export the model element file as?\n",
					"Export Model Element As",
					JOptionPane.PLAIN_MESSAGE,
					Icons.icons.get("Ok-32x32.png"),					
					possibilities,
					"bc");
		
		
		if ((s != null) && (s.length() > 0)) {		    		
		
			TreePath path=WorkbenchArea.tree.tree.getSelectionPath();
			
		    if(path!=null){		            
		    	WorkbenchArea.tree.tree.getSelectionModel().setSelectionPath(path);
		    	DefaultMutableTreeNode selectedNode=(DefaultMutableTreeNode)path.getLastPathComponent();		            	   
		    	Object nodeInfo=selectedNode.getUserObject();
		    	
		    	if(nodeInfo instanceof BiochamModelElement){
		    		BiochamModelElement element=(BiochamModelElement)nodeInfo;
		    		element.exportElementAs(s);		    		
		    		element=null;
		    	}		    	
		    	nodeInfo=null;
		    	selectedNode=null;
		    }else{
		    	JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Select the working model first!","Warning",JOptionPane.WARNING_MESSAGE);
		    }
		    
		    path=null;
		}
		
		possibilities=null;
		s=null;
	}
	

	
	/**
	 * Saves model's parameters to a file. 
	 * 
	 * From the parameters in biocham context, 
	 * only the parameters and initial concentrations can be saved to a file or exported to another format.
	 * 
	 */
	public void saveParameters() {
		
		TreePath path=WorkbenchArea.tree.tree.getSelectionPath();		   
		if(path!=null){
			   
			WorkbenchArea.tree.tree.getSelectionModel().setSelectionPath(path);
			   DefaultMutableTreeNode selectedNode=(DefaultMutableTreeNode)path.getLastPathComponent();		            	   
			   Object nodeInfo=selectedNode.getUserObject();
			   if(nodeInfo instanceof BiochamModelElement){
				   BiochamModelElement element=(BiochamModelElement)nodeInfo;
				   element.saveToFile();
				   element=null;
			   }else{
				   JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Select the working model first!","Warning",JOptionPane.WARNING_MESSAGE);
			   }
		   
			   selectedNode=null;
			   nodeInfo=null;
		}else{
			   JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Select the working model first!","Warning",JOptionPane.WARNING_MESSAGE);
		}
		path=null;
		
	}
	


	/**
	 * Adds data to a model.
	 */
	public void addToModel() {
		
	
		
		DefaultMutableTreeNode o=getSelectedNode();
		if(o!=null){			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){
				Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        		Utils.fileChooser.setFileFilter(null);
				String rep=Utils.showOpenDialog(BiochamMainFrame.frame,"importBiocham");			
				if (rep!=null) {
				   File file = new File(rep); 
				   if(!file.isDirectory()){
					   m.loadParametersFromFile(file);
				   }else{
						JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Select a file, not a whole directory!", "Selection Warning",JOptionPane.WARNING_MESSAGE);
				   }
				   file=null;
				}
				rep=null;
			}
			
			m=null;
		}	
		
		o=null;
		
	}

	
	
	
	/**
	 * Opens the dialog thats shows the model kinetics.
	 * */
	public void showModelKinetics(){
		
		DefaultMutableTreeNode o=getSelectedNode();
		if(o!=null){			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){				
				m.sendToBiocham("list_ODE.\n","kinetics");
				m.getKineticsDialog().show(m);
			}
			m=null;
		}	
		o=null;
	}
	
	
	
	
	
	
	/**
	 * Opens a saving dialog for the model's kinetics.
	 * */
	public void exportModelKinetics(){
		
		DefaultMutableTreeNode o=getSelectedNode();
		if(o!=null){			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){				
				m.sendToBiocham("list_ODE.\n","kinetics");
				if(m.getKinetics()!=null){
					m.exportKinetics();
				}
			}
			m=null;
		}	
		o=null;
	}
	
	

	
	
	/**
	 * Generates graph od current model's abstraction( graph of influences, neighbourhood graph or function graph)
	 */
	public void doTyping() {
		
		DefaultMutableTreeNode o=getSelectedNode();		
		if(o!=null){
			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){
				m.setWhoPopup(null);
				m.sendToBiocham("draw_influences.\n","influencesGraph");	
			}
			m=null;
		}

		o=null;
	}

	
	
	
	
	
	
	/**
	 * Opens the section for model checking
	 */
	public static void openModelChecking() {
		
		DefaultMutableTreeNode o=getSelectedNode();
		if(o!=null){			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){
				
			    JPanel panel=m.getCtlSpecifications().getParametersPanel();
		    	ArrayList<Component> cs;
		    	Component[] comps=panel.getComponents();												        	
				cs=new ArrayList<Component>();
				for(int i=0;i<comps.length;i++){
					if((comps[i] instanceof JButton)){
						if(!((JButton)comps[i]).getActionCommand().equals("addCTL") && !((JButton)comps[i]).getActionCommand().equals("addGenCTL")
								&& !((JButton)comps[i]).getActionCommand().equals("genCTL") && !((JButton)comps[i]).getActionCommand().equals("checkCTL") && !((JButton)comps[i]).getActionCommand().equals("nusmv")
								&& !((JButton)comps[i]).getActionCommand().equals("reduceModel")&& !((JButton)comps[i]).getActionCommand().equals("learnRules") 
								&& !((JButton)comps[i]).getActionCommand().equals("reviseModel")){
							
							cs.add(comps[i]);
						}    													
					}else if(!(comps[i] instanceof JLabel)){
						cs.add(comps[i]);
					}
				}
				ArrayList<Component> cs2=new ArrayList<Component>();
				JPanel panelLTLSpecs=m.getLtlSpecifications().getParametersPanel();
				Component[] comps2=panelLTLSpecs.getComponents();
				for(int i=0;i<comps2.length;i++){
					if((comps2[i] instanceof JButton)){
						if(!((JButton)comps2[i]).getActionCommand().equals("addLTL") && !((JButton)comps2[i]).getActionCommand().equals("checkLTL") 
								&& !((JButton)comps2[i]).getActionCommand().equals("searchParams") && !((JButton)comps2[i]).getActionCommand().equals("traceAnalyze")){
							
							cs2.add(comps2[i]);
						}
					}else if(!(comps2[i] instanceof JLabel)){
						cs2.add(comps2[i]);
					}
				}
				panel.removeAll();
				panelLTLSpecs.removeAll();			
				panel=((ParamTableCTLSpecifications)m.getCtlSpecifications().getParamTable()).refreshPanel(cs);		
				panelLTLSpecs=((ParamTableLTLSpecifications)m.getLtlSpecifications().getParamTable()).refreshPanel(cs2);
				cs.clear();
				cs2.clear();
				cs=null;
				cs2=null;
				int numComps1=panel.getComponentCount(),numComps2=panelLTLSpecs.getComponentCount();
				int dynHeight1=(numComps1/3)*40,dynHeight2=(numComps2/3)*40;
				
				JPanel p11=((ParamTableCTLSpecifications)m.getCtlSpecifications().getParamTable()).getCTLModelChecking();
				JPanel p22=((ParamTableLTLSpecifications)m.getLtlSpecifications().getParamTable()).getLTLModelChecking();
				
				panel.setPreferredSize(new Dimension(700,dynHeight1+200));
				panelLTLSpecs.setPreferredSize(new Dimension(700,dynHeight2+200));						

				UniversalScrollPane ctlSPEC=new UniversalScrollPane(panel);
				UniversalScrollPane ltlSPEC=new UniversalScrollPane(panelLTLSpecs);
				
				JPanel ctlCmds=((ParamTableCTLSpecifications)m.getCtlSpecifications().getParamTable()).getCommandsPanel();
				ctlCmds.setPreferredSize(new Dimension(200,270));
				JPanel ltlCmds=((ParamTableLTLSpecifications)m.getLtlSpecifications().getParamTable()).getCommandsPanel();
		
				ltlCmds.setPreferredSize(new Dimension(250,350));
			
				UniversalScrollPane ctlCommands=new UniversalScrollPane(ctlCmds);
				UniversalScrollPane ltlCommands=new UniversalScrollPane(ltlCmds);
				ctlCmds.revalidate();
				ltlCmds.revalidate();
				
				UniversalScrollPane ctlresults=new UniversalScrollPane(p11);
				UniversalScrollPane ltlResults=new UniversalScrollPane(p22);
				//between results and  commands
				//between commands and specs
				JSplitPane upCTL=new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,ctlCommands,ctlSPEC);		
				upCTL.setResizeWeight(0.2);
				upCTL.setDividerLocation(0.5);
				upCTL.setDividerSize(1);
				upCTL.setContinuousLayout(true);
				
				
				JSplitPane upLTL=new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,ltlCommands,ltlSPEC);
				upLTL.setResizeWeight(0.3);
				upLTL.setDividerLocation(0.5);
				upLTL.setDividerSize(1);
				upLTL.setContinuousLayout(true);
				
				//between specs and  results/commands
			
				JSplitPane spCTL=new JSplitPane(JSplitPane.VERTICAL_SPLIT,upCTL,ctlresults);
				
				JSplitPane spLTL=new JSplitPane(JSplitPane.VERTICAL_SPLIT,upLTL,ltlResults);
				spCTL.setResizeWeight(0.5);						
				spLTL.setResizeWeight(0.5);
				spCTL.setDividerLocation(0.5);
				spLTL.setDividerLocation(0.5);  
				int cnt=0;
				int tabs=WorkbenchArea.tabbedPane.getTabCount();
				for(int i=0;i<tabs;i++){
					 
					 String name=WorkbenchArea.tabbedPane.getTitleAt(cnt);					
					 if(!name.contains("Cmaes") && !name.contains("ODE-") && !name.contains("Boolean-") && !name.contains("Stochastic-") && !name.contains("Reaction Graph") && !name.contains("Influences Graph")){
						 WorkbenchArea.tabbedPane.remove(cnt);						
					 }else{
						 cnt++;
					 }
					 name=null;
				 }
				BiochamDynamicTree.workbench.returnTabbedPane();
				CTLView v=((ParamTableCTLSpecifications)BiochamDynamicTree.currentModel.getCtlSpecifications().getParamTable()).getView();
				WorkbenchArea.tabbedPane.add("Boolean Temporal Properties",v);		
				WorkbenchArea.tabbedPane.add("Numerical Temporal Properties",spLTL);	
				WorkbenchArea.tabbedPane.setSelectedIndex(WorkbenchArea.tabbedPane.getTabCount()-2);			
				
				panel=null;
				cs=null;
				comps=null;
				panelLTLSpecs=null;
				cs2=null;
				comps2=null;
				p11=null;
				p22=null;
				ctlSPEC=null; ltlSPEC=null; ltlCmds=null; ctlCmds=null;
				ctlCommands=null; ltlCommands=null; ctlresults=null; ltlResults=null;
				upLTL=null; spLTL=null;
				upCTL=null; spCTL=null;
			}
		}
		o=null;
	}

	
	
	
	
	
	
	
	/**
	 * Saves the model under new name
	 */
	public void saveModelAs() {
		
		DefaultMutableTreeNode o=getSelectedNode();
		if(o!=null){			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){
				m.saveToFile(false);						
				if(BiochamDynamicTree.nodeIndex>1){
					
				}	
			}
			m=null;
		}
		o=null;
	}


	
	
	
	
	
	
	
	/**
	 * Saves the state of the model
	 */
	public void saveModel() {
		
		
		DefaultMutableTreeNode o=getSelectedNode();
		if(o!=null){
			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){
				if(m.getModelFile()!=null){
					m.saveToFile(true);	
				}else {
					m.saveToFile(false);	
				}
				if(BiochamDynamicTree.nodeIndex>1){
					
				}
			}
			m=null;
		}
		o=null;
	}
	
	
	
	
	
	
	
	
	/**
	 * Adds a node to the Biocham Models' Explorer tree	  	
	 * @param parent The Model's node
	 * @param child The Model Element that should be added to the Tree explorer(LTL/CTL Temporal Properties, Simulations, etc, if any.)
	 * 
	 */	
    public DefaultMutableTreeNode addNodeObject(DefaultMutableTreeNode parent, Object child) {
        return addNodeObject(parent, child, false);
    }
    
    
    
    
    
    
    
    
    
    
    /**
     * Sub-method, called by addNodeObject(node, userObject) and addRuleObject(userObject).
	 * Adds a node to the Explorer tree
	 * @param parent The Model's node
	 * @param child The Model Element that should be added to the Tree explorer
	 * @param shouldBeVisible If the added node should be immediately visible
	 * 
	 */
    public DefaultMutableTreeNode addNodeObject(DefaultMutableTreeNode parent,Object child,boolean shouldBeVisible) {
        		 
    	DefaultMutableTreeNode childNode =  new DefaultMutableTreeNode(child);    	    	
        if (parent == null) {		        	
            parent = (DefaultMutableTreeNode)WorkbenchArea.tree.tree.getModel().getRoot();
        }       
        ((DefaultTreeModel)WorkbenchArea.tree.tree.getModel()).insertNodeInto(childNode, parent,parent.getChildCount());	
        if (shouldBeVisible) {		
        	TreePath path=new TreePath(childNode.getPath());
        	WorkbenchArea.tree.tree.scrollPathToVisible(path);
        	path=null;
        }
        return childNode;
    }
	

    
    
    
    
    
    
	/**
	 * Opens an empty Biocham model.
	 */
	public void newBCmodel() {
		
		
		TreePath path=WorkbenchArea.tree.tree.getSelectionPath();
		DefaultMutableTreeNode newModel = null;
		boolean notSelected=false;
		if(BiochamDynamicTree.openedModels.size()==0){
			BiochamMainFrame.frame.getContentPane().add(WorkbenchArea.wtb.getSecondToolBar(),CustomLayoutAKDock.NORTH);
		}
		if(path!=null){
			
			WorkbenchArea.tree.tree.getSelectionModel().setSelectionPath(path);
			DefaultMutableTreeNode selectedNode=(DefaultMutableTreeNode)path.getLastPathComponent();
			
			
			
			if(selectedNode.isRoot()){
				
				newModel=addNodeObject(selectedNode,new BiochamModel(BiochamDynamicTree.newNodeSuffix,WorkbenchArea.tree));
				lastParent=selectedNode;
				BiochamModel model=(BiochamModel) newModel.getUserObject();
				model.setModelNodeIndex(BiochamDynamicTree.nodeIndex);
				model.setSaved(true);
				BiochamDynamicTree.addToOpenedModels(BiochamDynamicTree.nodeIndex,model);
				BiochamDynamicTree.addToOpenedNodes(BiochamDynamicTree.nodeIndex,newModel);
				model.initializeModelElements(newModel,BiochamDynamicTree.workbench,null);
				BiochamDynamicTree.nodeIndex++;
				BiochamDynamicTree.newNodeSuffix++;	
				setWorkbenchLabelsOnNew(notSelected);
				expandAll(true);
				BiochamDynamicTree.currentModel=model;
				
				model=null;
				Utils.debugMsg("\n\n\n\nnewBCmodel: Set CurrentModel="+BiochamDynamicTree.currentModel.getModelName()+"\n\n\n\n");
			}else if(selectedNode.getUserObject() instanceof BiochamModel){
				DefaultMutableTreeNode root=(DefaultMutableTreeNode) selectedNode.getParent();
				newModel=this.addNodeObject(root,new BiochamModel(BiochamDynamicTree.newNodeSuffix,WorkbenchArea.tree));
				lastParent=root;
				BiochamModel model=(BiochamModel) newModel.getUserObject();
				model.setModelNodeIndex(BiochamDynamicTree.nodeIndex);
				model.setSaved(true);
				BiochamDynamicTree.addToOpenedModels(BiochamDynamicTree.nodeIndex,model);
				BiochamDynamicTree.addToOpenedNodes(BiochamDynamicTree.nodeIndex,newModel);
				model.initializeModelElements(newModel,BiochamDynamicTree.workbench, null);
				BiochamDynamicTree.nodeIndex++;		
				BiochamDynamicTree.newNodeSuffix++;	
				setWorkbenchLabelsOnNew(notSelected);
				expandAll(true);
				BiochamDynamicTree.currentModel=model;
				
				root=null;
				model=null;
				Utils.debugMsg("\n\n\n\nnewBCmodel: Set CurrentModel="+BiochamDynamicTree.currentModel.getModelName()+"\n\n\n\n");
			}else if(selectedNode.getUserObject() instanceof BiochamModelElement || selectedNode.getUserObject() instanceof AbstractionView || selectedNode.getUserObject() instanceof SimulationView){
				DefaultMutableTreeNode root1=(DefaultMutableTreeNode) selectedNode.getParent();
				DefaultMutableTreeNode root=(DefaultMutableTreeNode) root1.getParent();
				newModel=this.addNodeObject(root,new BiochamModel(BiochamDynamicTree.newNodeSuffix,WorkbenchArea.tree));
				lastParent=root;
				BiochamModel model=(BiochamModel) newModel.getUserObject();
				model.setModelNodeIndex(BiochamDynamicTree.nodeIndex);
				model.setSaved(true);
				BiochamDynamicTree.addToOpenedModels(BiochamDynamicTree.nodeIndex,model);
				BiochamDynamicTree.addToOpenedNodes(BiochamDynamicTree.nodeIndex,newModel);
				model.initializeModelElements(newModel,BiochamDynamicTree.workbench, null);
				BiochamDynamicTree.nodeIndex++;		
				BiochamDynamicTree.newNodeSuffix++;	
				setWorkbenchLabelsOnNew(notSelected);
				expandAll(true);
				BiochamDynamicTree.currentModel=model;
				Utils.debugMsg("\n\n\n\nnewBCmodel: Set CurrentModel="+BiochamDynamicTree.currentModel.getModelName()+"\n\n\n\n");
				root1=null;
				root=null;
				model=null;
			}else if(((DefaultMutableTreeNode)selectedNode.getParent()).getUserObject() instanceof BiochamDynamicTreeSeparator){
				
				   DefaultMutableTreeNode root2=(DefaultMutableTreeNode) selectedNode.getParent();// modelElement
				   DefaultMutableTreeNode root1=(DefaultMutableTreeNode) root2.getParent();//model
				   DefaultMutableTreeNode root=(DefaultMutableTreeNode) root1.getParent();//root
				   newModel=this.addNodeObject(root,new BiochamModel(BiochamDynamicTree.newNodeSuffix,WorkbenchArea.tree));
				   lastParent=root;
				   BiochamModel model=(BiochamModel) newModel.getUserObject();				   
				   model.setModelNodeIndex(BiochamDynamicTree.nodeIndex);
				   model.setSaved(true);
				   BiochamDynamicTree.addToOpenedModels(BiochamDynamicTree.nodeIndex,model);
				   BiochamDynamicTree.addToOpenedNodes(BiochamDynamicTree.nodeIndex,newModel);
				   model.initializeModelElements(newModel,BiochamDynamicTree.workbench, null);
				   BiochamDynamicTree.nodeIndex++;		
				   BiochamDynamicTree.newNodeSuffix++;
				   setWorkbenchLabelsOnNew(notSelected);
				   expandAll(true);
				   BiochamDynamicTree.currentModel=model;
				   root1=null;
				   root=null;
				   model=null;
				   root2=null;
				   Utils.debugMsg("\n\n\n\nnewBCmodel: Set CurrentModel="+BiochamDynamicTree.currentModel.getModelName()+"\n\n\n\n");
			}else if(selectedNode.getUserObject() instanceof BiochamDynamicTreeSeparator){
				DefaultMutableTreeNode root1=(DefaultMutableTreeNode) selectedNode.getParent();
				DefaultMutableTreeNode root=(DefaultMutableTreeNode) root1.getParent();
				newModel=this.addNodeObject(root,new BiochamModel(BiochamDynamicTree.newNodeSuffix,WorkbenchArea.tree));
				lastParent=root;
				BiochamModel model=(BiochamModel) newModel.getUserObject();
				model.setModelNodeIndex(BiochamDynamicTree.nodeIndex);
				model.setSaved(true);
				BiochamDynamicTree.addToOpenedModels(BiochamDynamicTree.nodeIndex,model);
				BiochamDynamicTree.addToOpenedNodes(BiochamDynamicTree.nodeIndex,newModel);
				model.initializeModelElements(newModel,BiochamDynamicTree.workbench, null);
				BiochamDynamicTree.nodeIndex++;		
				BiochamDynamicTree.newNodeSuffix++;
				setWorkbenchLabelsOnNew(notSelected);
				expandAll(true);
				BiochamDynamicTree.currentModel=model;
				Utils.debugMsg("\n\n\n\nnewBCmodel: Set CurrentModel="+BiochamDynamicTree.currentModel.getModelName()+"\n\n\n\n");
				root1=null;
				root=null;
				model=null;
			}
			else{
				JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Select a model from the Explorer Tree.","Notification",JOptionPane.INFORMATION_MESSAGE);
				notSelected=true;
			}	
		}else{
			 if(lastParent!=null){
				   newModel=this.addNodeObject(lastParent,new BiochamModel(BiochamDynamicTree.newNodeSuffix,WorkbenchArea.tree));				   
				   BiochamModel model=(BiochamModel) newModel.getUserObject();
				   BiochamDynamicTree.currentModel=model;
				   Utils.debugMsg("\n\n\n\nnewBCmodel: Set CurrentModel="+BiochamDynamicTree.currentModel.getModelName()+"\n\n\n\n");
				   model.setModelNodeIndex(BiochamDynamicTree.nodeIndex);
				   model.setSaved(true);
				   BiochamDynamicTree.addToOpenedModels(BiochamDynamicTree.nodeIndex,model);
				   BiochamDynamicTree.addToOpenedNodes(BiochamDynamicTree.nodeIndex,newModel);
				   model.initializeModelElements(newModel,BiochamDynamicTree.workbench, null);
				   BiochamDynamicTree.nodeIndex++;		
				   BiochamDynamicTree.newNodeSuffix++;
				   setWorkbenchLabelsOnNew(notSelected);
				   expandAll(true);
				   model=null;
			   }
		}
		
		TreePath selectionPath=new TreePath(newModel.getPath());
		WorkbenchArea.tree.tree.setSelectionPath(selectionPath);   
		BiochamMenuBar.setMenusEnabled(true);
		path=null;
		selectionPath=null;
		newModel = null;
	}

	
	
	
	
	
	
	
	/**
	 * @param notSelected
	 */
	private void setWorkbenchLabelsOnNew(boolean notSelected) {
		
	  	WorkbenchArea.wtb.setOpened(true);
		WorkbenchArea.wtb.setSaved(true);	
		
	}

	
	
	
	
	
	
	/**
	 * Opens an existing model from a file. It can be from a biocham format(bc) or sbml format(xml)
	 */
	public void openBCmodel() {
		
	
		
		if(BiochamDynamicTree.firstLoad){
			Utils.fileChooser.setCurrentDirectory(Utils.getEXAMPLES_DIR());
			BiochamDynamicTree.firstLoad=false;
		}
		Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		String rep=Utils.showOpenDialog(BiochamMainFrame.frame,"importBiocham");			
		if (rep!=null && rep!="") {
			File file=new File(rep);
			if(!file.isDirectory()){
			   TreePath path=WorkbenchArea.tree.tree.getSelectionPath();
			   boolean notSelected=false;
			   Utils.debugMsg("From JFileChooser filepath is: "+rep+".\nOtherwise forn new File(rep) is: "+file.getAbsolutePath());
			   Utils.debugMsg("Tree selection path is: "+path);
			   openingBiochamModel(file, path, notSelected);
			}else{
				JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Select a file, not a whole directory!", "Selection Warning",JOptionPane.WARNING_MESSAGE);
			}
			file=null;
			
		}
		rep=null;
		
	}

	
	
	
	
	
	
	
	
	
	
	/**
	 * Opens an existing model from a file. It can be from a biocham format(bc) or sbml format(xml).
	 */
	public void openBCmodel(String filepath) {
		
		if (filepath!=null) {
			
			File file=new File(filepath);
			TreePath path=WorkbenchArea.tree.tree.getSelectionPath();
			boolean notSelected=false;		   
			openingBiochamModel(file, path, notSelected);
			path=null;
			file=null;
		}
	}

	
	
	
	
	
	
	
	
	
	/**
	 * @param file
	 * @param path
	 * @param notSelected
	 */
	private void openingBiochamModel(File file, TreePath path, boolean notSelected) {
		
		DefaultMutableTreeNode newModel = null;
		if(BiochamDynamicTree.openedModels.size()==0){
			BiochamMainFrame.frame.getContentPane().add(WorkbenchArea.wtb.getSecondToolBar(),CustomLayoutAKDock.NORTH);
		}
		
		if(path!=null){
			   
			WorkbenchArea.tree.tree.getSelectionModel().setSelectionPath(path);
			
			DefaultMutableTreeNode selectedNode=(DefaultMutableTreeNode)path.getLastPathComponent();
			Utils.debugMsg("SELECTED OBJECT: "+selectedNode.getUserObject());
			if(selectedNode.isRoot()){
				   newModel=this.addNodeObject(selectedNode,new BiochamModel(file,WorkbenchArea.tree));
				   lastParent=selectedNode;
				   
				   BiochamModel model=(BiochamModel) newModel.getUserObject();
				   if(model!=null){
					   if(model.getRules()!=null){
						   if(model.getRules().getParamTable()!=null){
							   ((ParamTableRules)model.getRules().getParamTable()).setIgnoreUndefinedParametersWarnings(true);
						   }
					   }
				   }
				   
				   model.setModelNodeIndex(BiochamDynamicTree.nodeIndex);
				   model.setSaved(true);
				   BiochamDynamicTree.addToOpenedModels(BiochamDynamicTree.nodeIndex,model);
				   BiochamDynamicTree.addToOpenedNodes(BiochamDynamicTree.nodeIndex,newModel);
				   model.initializeModelElements(newModel,BiochamDynamicTree.workbench, file);		 
				   BiochamDynamicTree.nodeIndex++;
				   setWorkbenchLabelsOnOpen(notSelected);		
				   expandAll(true);
				   BiochamDynamicTree.currentModel=model;
				   Utils.debugMsg("\n\n\n\nopeningBiochamModel: Set CurrentModel="+BiochamDynamicTree.currentModel.getModelName()+"\n\n\n\n");
				   model=null;				   
			   }else if(selectedNode.getUserObject() instanceof BiochamModel ){
				   DefaultMutableTreeNode root=(DefaultMutableTreeNode) selectedNode.getParent();
				   newModel=this.addNodeObject(root,new BiochamModel(file,WorkbenchArea.tree));
				   lastParent=root;
				   BiochamModel model=(BiochamModel) newModel.getUserObject();				   
				   model.setModelNodeIndex(BiochamDynamicTree.nodeIndex);
				   model.setSaved(true);
				   BiochamDynamicTree.addToOpenedModels(BiochamDynamicTree.nodeIndex,model);
				   BiochamDynamicTree.addToOpenedNodes(BiochamDynamicTree.nodeIndex,newModel);
				   model.initializeModelElements(newModel,BiochamDynamicTree.workbench, file);
				   BiochamDynamicTree.nodeIndex++;		 
				   setWorkbenchLabelsOnOpen(notSelected);		
				   expandAll(true);
				   BiochamDynamicTree.currentModel=model;
				   Utils.debugMsg("\n\n\n\nopeningBiochamModel: Set CurrentModel="+BiochamDynamicTree.currentModel.getModelName()+"\n\n\n\n");
				   model=null;				 
				   root=null;
			   }else if(selectedNode.getUserObject() instanceof BiochamModelElement || selectedNode.getUserObject() instanceof AbstractionView || selectedNode.getUserObject() instanceof SimulationView){
				   DefaultMutableTreeNode root1=(DefaultMutableTreeNode) selectedNode.getParent();
				   DefaultMutableTreeNode root=(DefaultMutableTreeNode) root1.getParent();
				   newModel=this.addNodeObject(root,new BiochamModel(file,WorkbenchArea.tree));
				   lastParent=root;
				   BiochamModel model=(BiochamModel) newModel.getUserObject();				   
				   model.setModelNodeIndex(BiochamDynamicTree.nodeIndex);
				   model.setSaved(true);
				   BiochamDynamicTree.addToOpenedModels(BiochamDynamicTree.nodeIndex,model);
				   BiochamDynamicTree.addToOpenedNodes(BiochamDynamicTree.nodeIndex,newModel);
				   model.initializeModelElements(newModel,BiochamDynamicTree.workbench, file);
				   BiochamDynamicTree.nodeIndex++;	
				   setWorkbenchLabelsOnOpen(notSelected);		
				   expandAll(true);
				   BiochamDynamicTree.currentModel=model;
				   Utils.debugMsg("\n\n\n\nopeningBiochamModel: Set CurrentModel="+BiochamDynamicTree.currentModel.getModelName()+"\n\n\n\n");
				   model=null;
				   root1=null;
				   root=null;
			   }else if(((DefaultMutableTreeNode)selectedNode.getParent()).getUserObject() instanceof BiochamDynamicTreeSeparator){
				   DefaultMutableTreeNode root2=(DefaultMutableTreeNode) selectedNode.getParent();
				   DefaultMutableTreeNode root1=(DefaultMutableTreeNode) root2.getParent();
				   DefaultMutableTreeNode root=(DefaultMutableTreeNode) root1.getParent();
				   newModel=this.addNodeObject(root,new BiochamModel(file,WorkbenchArea.tree));
				   lastParent=root;
				   BiochamModel model=(BiochamModel) newModel.getUserObject();				   
				   model.setModelNodeIndex(BiochamDynamicTree.nodeIndex);
				   model.setSaved(true);
				   BiochamDynamicTree.addToOpenedModels(BiochamDynamicTree.nodeIndex,model);
				   BiochamDynamicTree.addToOpenedNodes(BiochamDynamicTree.nodeIndex,newModel);
				   model.initializeModelElements(newModel,BiochamDynamicTree.workbench, file);
				   BiochamDynamicTree.nodeIndex++;
				   setWorkbenchLabelsOnOpen(notSelected);		
				   expandAll(true);
				   BiochamDynamicTree.currentModel=model;
				   Utils.debugMsg("\n\n\n\nopeningBiochamModel: Set CurrentModel="+BiochamDynamicTree.currentModel.getModelName()+"\n\n\n\n");
				   model=null;
				   root1=null;
				   root=null;
				   root2=null;
			   }else if(selectedNode.getUserObject() instanceof BiochamDynamicTreeSeparator){
				   DefaultMutableTreeNode root1=(DefaultMutableTreeNode) selectedNode.getParent();
				   DefaultMutableTreeNode root=(DefaultMutableTreeNode) root1.getParent();
				   newModel=this.addNodeObject(root,new BiochamModel(file,WorkbenchArea.tree));
				   lastParent=root;
				   BiochamModel model=(BiochamModel) newModel.getUserObject();				   
				   model.setModelNodeIndex(BiochamDynamicTree.nodeIndex);
				   model.setSaved(true);
				   BiochamDynamicTree.addToOpenedModels(BiochamDynamicTree.nodeIndex,model);
				   BiochamDynamicTree.addToOpenedNodes(BiochamDynamicTree.nodeIndex,newModel);
				   model.initializeModelElements(newModel,BiochamDynamicTree.workbench, file);
				   BiochamDynamicTree.nodeIndex++;	
				   setWorkbenchLabelsOnOpen(notSelected);		
				   expandAll(true);
				   BiochamDynamicTree.currentModel=model;
				   Utils.debugMsg("\n\n\n\nopeningBiochamModel: Set CurrentModel="+BiochamDynamicTree.currentModel.getModelName()+"\n\n\n\n");
				   model=null;
				   root1=null;
				   root=null;
			   }else{
					JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Select a model from the Explorer Tree.","Notification",JOptionPane.INFORMATION_MESSAGE);
					notSelected=true;
			   }	
			   selectedNode=null;
		   
		   }else{
			   if(lastParent!=null){
				   newModel=this.addNodeObject(lastParent,new BiochamModel(file,WorkbenchArea.tree));				   
				   BiochamModel model=(BiochamModel) newModel.getUserObject();				   
				   model.setModelNodeIndex(BiochamDynamicTree.nodeIndex);
				   model.setSaved(true);
				   BiochamDynamicTree.addToOpenedModels(BiochamDynamicTree.nodeIndex,model);
				   BiochamDynamicTree.addToOpenedNodes(BiochamDynamicTree.nodeIndex,newModel);
				   model.initializeModelElements(newModel,BiochamDynamicTree.workbench, file);
				   BiochamDynamicTree.nodeIndex++;	
				   setWorkbenchLabelsOnOpen(notSelected);		
				   expandAll(true);
				   BiochamDynamicTree.currentModel=model;
				   Utils.debugMsg("\n\n\n\nopeningBiochamModel: Set CurrentModel="+BiochamDynamicTree.currentModel.getModelName()+"\n\n\n\n");
				   model=null;
			   }
		   }
		TreePath selectionPath=new TreePath(newModel.getPath());
		WorkbenchArea.tree.tree.setSelectionPath(selectionPath); 
		BiochamMenuBar.setMenusEnabled(true);
		   
		newModel=null;
		selectionPath=null;
	}

	
	
	
	
	
	
	
	
	
	
	
	/**
	 * @param notSelected
	 */
	private void setWorkbenchLabelsOnOpen(boolean notSelected) {
		
		  WorkbenchArea.wtb.setOpened(true);
		  WorkbenchArea.wtb.setSaved(true);
	}
	

	
	
	
	
	
	
	
	
	
	
	
	
	/**
	 * Removes the current node from the tree explorer and disposes the model
	 */
	public void removeCurrentNode() {
				
		DefaultMutableTreeNode o=getSelectedNode();
		if(o!=null){			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){				
				 int k=disposeModel(m);
				 if(k>=0){
					
						Object key=Utils.getKeyFromValue(BiochamDynamicTree.openedModels,m);
						int ik=-1;
						try{
							ik=Integer.parseInt(key.toString());	
						}catch(Exception e){
							ik=-1;
							Utils.debugMsg("Error in conversion and finding the model index.");
						}
						 if(ik>-1){
							 BiochamDynamicTree.openedModels.remove(ik);
							   DefaultMutableTreeNode lastChild=getNodeSeparatorInstance(o);
							   ((DefaultTreeModel)WorkbenchArea.tree.tree.getModel()).removeNodeFromParent((DefaultMutableTreeNode)lastChild.getParent());							         
							   WorkbenchArea.wtb.setIntro(true);
							   expandAll(true);
							   lastChild=null;
						 }
				 }
				 m=null;
			}
		}			
		if(BiochamDynamicTree.openedModels.size()==0){
			BiochamMenuBar.setMenusEnabled(false);					
			BiochamDynamicTree.workbench.wtb.setOpened(false);
			BiochamDynamicTree.workbench.wtb.setSaved(false);
			BiochamDynamicTree.workbench.wtb.setIntro(true);			
		}else{
			BiochamMenuBar.setMenusEnabled(true);					
			BiochamDynamicTree.workbench.wtb.setOpened(true);
			BiochamDynamicTree.workbench.wtb.setSaved(true);
			BiochamDynamicTree.workbench.wtb.setIntro(true);
		}
		o=null;		
	}

	
	
	
	
	
	
	
	
	
	/**
	 * Calls the model's restart method.
	 * */
	public void restartBiochamProcess() {
	
		
		DefaultMutableTreeNode o=getSelectedNode();
		if(o!=null){			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){
				m.restartBiochamProcess();
			}
			m=null;
		}
		o=null;
	}

	    

	
	
	
	/**
	 * Properly closes the model by freeing its resources and disconnecting it from Biocham
	 * 
	 * @param mdl Model which has to be closed
	 */
	private int disposeModel(BiochamModel mdl) {
			
		
	
			   if(mdl!=null){
				   int resp=JOptionPane.showConfirmDialog(BiochamMainFrame.frame, "Do you want to save the model before closing?", "Question", JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE);
				   if(resp!=JOptionPane.CANCEL_OPTION){
					   if(resp==JOptionPane.YES_OPTION){
						   mdl.saveToFile(false);
					   }						   
					   mdl.disposeBiochamModel();					  
				   }else{
					   return -1;
				   }
			   }
		   return 0;
	}	
	
	
	
	
	
	/**
	 * Opens a dialog for executing a numerical simulation.
	 * 
	 * */
	public void numericalSimulation() {
		DefaultMutableTreeNode o=getSelectedNode();
		if(o!=null){			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){				
				new DialogSimulation(m);		
			}			
			m=null;
		}else{
			JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Select the working model first!","Warning",JOptionPane.WARNING_MESSAGE);
		}
		o=null;
	}
	
	
	
	
	
	/**
	 * Tries to remove all opened models's tabs???????????
	 * 
	 * */
	public static void removeAllOpenedInstances() {
		for(int i=0;i<BiochamDynamicTree.openedModels.size();i++){
			BiochamModel m=BiochamDynamicTree.openedModels.get(i);
			if(m!=null){
				for(int j=0;j<m.getModelEditor().getTabbedPane().getTabCount();j++){
					if(m.getModelEditor().getTabbedPane().getTitleAt(j).contains("Compare Simulation")){
						m.getModelEditor().getTabbedPane().removeTabAt(j);
					}
				}
				for(int j=0;j<WorkbenchArea.tabbedPane.getTabCount();j++){
					if(WorkbenchArea.tabbedPane.getTitleAt(j).contains("Compare Simulation")){
						WorkbenchArea.tabbedPane.removeTabAt(j);
					}
				}
				
			}
		}
		BiochamDynamicTree.compWindowAlreadyOpened=false;
	}	
	
	

	public void booleanSimDialog() {
		DefaultMutableTreeNode o=getSelectedNode();
		if(o!=null){			
			BiochamModel m = getModelInstanceFromSelectedNode(o);			
			if(m!=null){				
				//m.backupModel();
				new DialogSimulation(m);		
			}			
			m=null;
		}else{
			JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Select the working model first!","Warning",JOptionPane.WARNING_MESSAGE);
		}
		o=null;
		
	}
	

	public void stochasticSimDialog() {
		// TODO Auto-generated method stub
		
	}
	

	public void odeSimDialog() {
		// TODO Auto-generated method stub
		
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
}
