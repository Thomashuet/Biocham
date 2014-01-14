package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.event.GraphSelectionEvent;
import org.jgraph.event.GraphSelectionListener;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.GraphUndoManager;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.utils.BrowserLauncher;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;



import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;

import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JToolBar;
import javax.swing.event.UndoableEditEvent;

public class GraphEditorToolbar extends MouseAdapter implements GraphSelectionListener, ActionListener {

	
	JToolBar firstToolBar, secondToolbar;
	JButton stateTransition, association, dissociation, addReactant, addProduct, addModulator, addCompartment;//, multimerization;// macromolecule, gene, source_sink, 
	JButton undo, redo, zoomIn, zoomOut, remove;
//	JLabel toBack,toFront; 
	JButton cut,copy,paste;
	BiochamGraph graph;
	GraphUndoManager undoManager;
	Action copyAction, cutAction, pasteAction;
	
	
	public GraphEditorToolbar(BiochamGraph graphInstance){
		
		this.graph=graphInstance;
		firstToolBar=createToolbar("graphEditorToolbar1");
		secondToolbar=createToolbar("graphEditorToolbar2");
		
		graph.getSelectionModel().addGraphSelectionListener(this);
		graph.getModel().addGraphModelListener(graph.getGraphEditorInstance().getStatusBarListener());		
		undoManager = new GraphUndoManager() {		
			public void undoableEditHappened(UndoableEditEvent e) {
				super.undoableEditHappened(e);				
				updateHistoryButtons();
			}
		};		
		graph.getSelectionModel().addGraphSelectionListener(this);	
		graph.addKeyListener(new KeyAdapter(){
			public void keyPressed(KeyEvent e) {				
				if (e.getKeyCode() == KeyEvent.VK_DELETE){
					remove();
				}else if(e.getKeyCode() == KeyEvent.VK_COPY){
					copyAction.actionPerformed(new ActionEvent(graph, e.getID(),"copy", e.getModifiers()));					
				}else if(e.getKeyCode() == KeyEvent.VK_CUT){
					cutAction.actionPerformed(new ActionEvent(graph, e.getID(), "cut", e.getModifiers()));
					paste.setEnabled(true);
				}else if(e.getKeyCode() == KeyEvent.VK_PASTE){
					pasteAction.actionPerformed(new ActionEvent(graph, e.getID(),"paste", e.getModifiers()));
					
				}else if(e.getKeyCode() == KeyEvent.VK_Z){
					undo();
				}else if(e.getKeyCode() == KeyEvent.VK_Y){
					redo();
				}
				
			}
		});
		graph.getModel().addUndoableEditListener(undoManager);
		cut=createToolbarButton(Icons.icons.get("cut2.png"+0.6),Icons.icons.get("cut2.png"+0.8),"Cut");
		copy=createToolbarButton(Icons.icons.get("copy.png"),Icons.icons.get("copy.png"+1.2),"Copy");
		paste=createToolbarButton(Icons.icons.get("pastee.png"),Icons.icons.get("pastee.png"+1.2),"Paste");
		copyAction=javax.swing.TransferHandler.getCopyAction();
		cutAction=javax.swing.TransferHandler.getCutAction();
		pasteAction=javax.swing.TransferHandler.getPasteAction();
	}
	
	
	




	public JToolBar getFirstToolBar(){
		
				
		stateTransition=createToolbarButton(Icons.icons.get("StateTransition1.png"+0.6),Icons.icons.get("StateTransition1.png"+0.65),"State Transition");    
		//stateTransition.setToolTipText("State Transition");	 
		stateTransition.setName("stateTransition");
		stateTransition.addMouseListener(this);
		firstToolBar.add(stateTransition);	
		addSeparator(firstToolBar);
		addSeparator(firstToolBar);
		
		association=createToolbarButton(Icons.icons.get("Association1.png"+0.6),Icons.icons.get("Association1.png"+0.65),"Association"); 
		association.setName("association");
		association.addMouseListener(this);
		firstToolBar.add(association);	
		addSeparator(firstToolBar);
		addSeparator(firstToolBar);
		
		dissociation=createToolbarButton(Icons.icons.get("Dissociation1.png"+0.6),Icons.icons.get("Dissociation1.png"+0.65),"Dissociation");    
		dissociation.setName("dissociation");
		dissociation.addMouseListener(this);
		firstToolBar.add(dissociation);	
		addSeparator(firstToolBar);
		addSeparator(firstToolBar);
		
		addReactant=createToolbarButton(Icons.icons.get("addReactant.png"+0.6),Icons.icons.get("addReactant.png"+0.65),"Add Reactant");  
		addReactant.setEnabled(true);
		addReactant.setName("addReactant");
		addReactant.addMouseListener(this);
		firstToolBar.add(addReactant);	
		addSeparator(firstToolBar);
		addSeparator(firstToolBar);
		
		addProduct=createToolbarButton(Icons.icons.get("addProduct.png"+0.6),Icons.icons.get("addProduct.png"+0.65),"Add Product");  
		addProduct.setName("addProduct");
		addProduct.setEnabled(true);
		addProduct.addMouseListener(this);
		firstToolBar.add(addProduct);	
		addSeparator(firstToolBar);
		addSeparator(firstToolBar);
		
		addModulator=createToolbarButton(Icons.icons.get("addModulator.png"+0.6),Icons.icons.get("addModulator.png"+0.65),"Add Modulator");  
		addModulator.setName("addModulator");
		addModulator.setEnabled(true);
		addModulator.addMouseListener(this);
		firstToolBar.add(addModulator);	
		addSeparator(firstToolBar);
		addSeparator(firstToolBar);
				
		
		addCompartment=createToolbarButton(Icons.icons.get("addCompartment.png"+0.15),Icons.icons.get("addCompartment.png"+0.2),"Add Compartment");  
		addCompartment.setName("addCompartment");
		addCompartment.setEnabled(true);
		addCompartment.addMouseListener(this);
		firstToolBar.add(addCompartment);	
		addSeparator(firstToolBar);
		addSeparator(firstToolBar);
		
		return firstToolBar;
		
	}
	
	
	public JToolBar getSecondToolBar(){
		
		
		// cut, copy, paste, undo, redo, zoomIn, zoomOut; 
	
		secondToolbar.add(cut);	
		addSeparator(secondToolbar);
		addSeparator(secondToolbar);
		secondToolbar.add(copy);
		addSeparator(secondToolbar);
		addSeparator(secondToolbar);
		secondToolbar.add(paste);	
		addSeparator(secondToolbar);
		addSeparator(secondToolbar);
		
		remove=createToolbarButton(Icons.icons.get("remove.png"),Icons.icons.get("remove.png"+1.2),"Remove");
		remove.setName("remove");
		remove.addMouseListener(this);
		secondToolbar.add(remove);	
		addSeparator(secondToolbar);
		addSeparator(secondToolbar);
		
		
		undo=createToolbarButton(Icons.icons.get("undoW.png"),Icons.icons.get("undoW.png"+1.2),"Undo");    
		undo.setName("undo");
		undo.addMouseListener(this);
		
		secondToolbar.add(undo);	
		addSeparator(secondToolbar);
		addSeparator(secondToolbar);
		
		redo=createToolbarButton(Icons.icons.get("redoW.png"),Icons.icons.get("redoW.png"+1.2),"Redo");    
		redo.setName("redo");
		redo.addMouseListener(this);
		secondToolbar.add(redo);	
		addSeparator(secondToolbar);
		addSeparator(secondToolbar);
		
		zoomIn=createToolbarButton(Icons.icons.get("ZoomIn.png"),Icons.icons.get("ZoomIn.png"+1.2),"Zoom In");   
		zoomIn.setName("zoomIn");
		zoomIn.addMouseListener(this);
		secondToolbar.add(zoomIn);	
		addSeparator(secondToolbar);
		addSeparator(secondToolbar);
		
		zoomOut=createToolbarButton(Icons.icons.get("ZoomOut.png"),Icons.icons.get("ZoomOut.png"+1.2),"Zoom Out");  
		zoomOut.setName("zoomOut");
		zoomOut.addMouseListener(this);
		secondToolbar.add(zoomOut);	
		addSeparator(secondToolbar);
		addSeparator(secondToolbar);
		
		
		
		return secondToolbar;
		
	}
	
	private JLabel addSeparator(JToolBar toolbar){
		JLabel sep=new JLabel("  ");
		
		sep.setBackground(Utils.backgroundColor);
		sep.setForeground(Utils.backgroundColor);
		toolbar.add(sep);
		return sep;
	}


	public void mouseClicked(MouseEvent e) {
		if(e.getSource() instanceof JButton){

			JButton l=(JButton)e.getSource();
			String cmd=l.getName();
			if(cmd!=null){
				if(cmd.equals("stateTransition")){
					graph.clearSelection();
					addStateTransition(graph);
									
					
				}else if(cmd.equals("association")){
					graph.clearSelection();
					addAssociation(graph);
					graph.clearSelection();
					
				}else if(cmd.equals("dissociation")){
					graph.clearSelection();
					addDissociation(graph);
					graph.clearSelection();
					
				}else if(cmd.equals("addReactant")){
					//graph.clearSelection();
					addReactant();
					graph.clearSelection();
					
				}else if(cmd.equals("addProduct")){
					//graph.clearSelection();
					addProduct();
					graph.clearSelection();
					
				}else if(cmd.equals("addModulator")){
					
					addModulator();					
					graph.clearSelection();
					
				}else if(cmd.equals("undo")){
					undo();
					
				}else if(cmd.equals("redo")){
					redo();
					
				}else if(cmd.equals("zoomIn")){
					zoomIn();
					
				}else if(cmd.equals("zoomOut")){
					zoomOut();
					
				}else if(cmd.equals("remove")){
					remove();
					
				}else if(cmd.equals("Reset colors")){
					GraphUtilities.resetColors(graph);
				}else if(cmd.contains("Compartment")){
					String newValue= (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,"Compartment name:\n",""); 
					if(newValue!=null && newValue!=""){
						BiochamCompartmentData data=new BiochamCompartmentData(graph);
						data.setCompartmentName(newValue);
						//data.setCompartmentName("Nucleus");
						//data.setColor(Utils.gradientViolet);
						ECompartmentCell compartment=new ECompartmentCell(data);
						graph.getGraphLayoutCache().insert(compartment);
						graph.getGraphLayoutCache().toBack(new Object[]{compartment});
						graph.setMoveIntoGroups(false);
					}	
					
					
					
				/*	BiochamCompartmentData data1=new BiochamCompartmentData(graph);
					data1.setCompartmentName("FSH-targeted cell");
					data1.setColor(Utils.gradientGray);
					graph.getGraphLayoutCache().insert(new ECompartmentCell(data1));*/

				}
			}
			
			cmd=null;
			l=null;
		
		}else if(e.getSource() instanceof JLabel){
			
			JLabel l=(JLabel)e.getSource();
			String cmd=l.getName();
			if(cmd!=null){
			
			if(cmd.equals("toBack")){
				if (!graph.isSelectionEmpty())
					toBack(graph.getSelectionCells());
				
			}else if(cmd.equals("toFront")){
				if (!graph.isSelectionEmpty())
					toFront(graph.getSelectionCells());
				
			}else if(cmd.contains("Export")){
				SwingWorker sw=new SwingWorker(){

					@Override
					public Object construct() {
						String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,"image");			
						if (rep!=null) {			
							if(!rep.contains(".")){
								rep+=".png";
							}
							graph.exportAsImage(rep);
						}
						return null;
					}

					@Override
					public void finished() {
						// TODO Auto-generated method stub
						
					}};
					sw.start();
			}
			}
		}else{
			
		}
	}

	public void toFront(Object[] c) {
		graph.getGraphLayoutCache().toFront(c);
	}

	// Sends the Specified Cells to Back
	public void toBack(Object[] c) {
		graph.getGraphLayoutCache().toBack(c);
	}
	
	public static void addStateTransition(BiochamGraph graph) {
		new ReactionDialog(graph,BiochamGraph.graphEditorFrame,"State Transition");
		
	}
	
	public static void addAssociation(BiochamGraph graph) {
		new ReactionDialog(graph,BiochamGraph.graphEditorFrame,"Association");
		
	}
	
	public static void addDissociation(BiochamGraph graph) {
		new ReactionDialog(graph,BiochamGraph.graphEditorFrame,"Dissociation");
		
	}
	
	public void addReactant() {
		if(!graph.isSelectionEmpty()){
			
			Object[] selection=graph.getSelectionCells();
			int len=selection.length;
			if(len==1){
				if(selection[0] instanceof StateTransition){
					StateTransition st=((StateTransition)selection[0]);
					boolean allowed=true;
					for(int i=0;i<st.getSources().size();i++){					
						if(st.getSources().get(i).getParentName().equals("Source/Sink")){
							allowed=false;
						}
					}
					if(allowed){
						st.addReactant();
					}else{
						JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You can't add a reactant when there is Source/Sink as a reactant already.","Warning",JOptionPane.WARNING_MESSAGE);
					}
					st=null;
				}else if(selection[0] instanceof Association){
					((Association)selection[0]).addReactant();
				}else if(selection[0] instanceof Dissociation){
					((Dissociation)selection[0]).addReactant();
				}else if(selection[0] instanceof ReversibleAssociation){
					ReversibleAssociation st=(ReversibleAssociation)selection[0];
					st.addReactant();
				}else if(selection[0] instanceof ReversibleStateTransition){
					ReversibleStateTransition st=(ReversibleStateTransition)selection[0];
					st.addReactant();
				}else{
					JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You have to select reaction not a molecule.","Warning",JOptionPane.WARNING_MESSAGE);
				}
			}else{
				JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You have to select JUST one reaction to which you wish to add a reactant.","Warning",JOptionPane.WARNING_MESSAGE);
			}
			
		}else{
			JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You have to select one reaction to which you wish to add a reactant.","Warning",JOptionPane.WARNING_MESSAGE);
		}
		
		
		
	}
	
	public void addProduct() {
		if(!graph.isSelectionEmpty()){
			
			Object[] selection=graph.getSelectionCells();
			int len=selection.length;
			if(len==1){
				if(selection[0] instanceof StateTransition){
					StateTransition st=((StateTransition)selection[0]);
					boolean allowed=true;
					for(int i=0;i<st.getTargets().size();i++){
						if(st.getTargets().get(i).getParentName().equals("Source/Sink")){
							allowed=false;
						}
					}
					if(allowed){
						st.addProduct();
					}else{
						JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You can't add a product when there is Source/Sink as a product already.","Warning",JOptionPane.WARNING_MESSAGE);
					}
					st=null;
				}else if(selection[0] instanceof Association){
					((Association)selection[0]).addProduct();
				}else if(selection[0] instanceof Dissociation){
					((Dissociation)selection[0]).addProduct();
				}else if(selection[0] instanceof ReversibleAssociation){
					ReversibleAssociation st=(ReversibleAssociation)selection[0];
					st.addProduct();
				}else if(selection[0] instanceof ReversibleStateTransition){
					ReversibleStateTransition st=(ReversibleStateTransition)selection[0];
					st.addProduct();
				}else{
					JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You have to select reaction not a molecule.","Warning",JOptionPane.WARNING_MESSAGE);
				}
			}else{
				JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You have to select JUST one reaction to which you wish to add a product.","Warning",JOptionPane.WARNING_MESSAGE);
			}
		}else{
			JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You have to select one reaction to which you wish to add a product.","Warning",JOptionPane.WARNING_MESSAGE);
		}
		
	}
	
	public void addModulator() {
		
		if(!graph.isSelectionEmpty()){
			
			Object[] selection=graph.getSelectionCells();
			int len=selection.length;
			if(len==1){
				if(selection[0] instanceof StateTransition){
					((StateTransition)selection[0]).addModulator();
				}else if(selection[0] instanceof Association){
					((Association)selection[0]).addModulator();
				}else if(selection[0] instanceof Dissociation){
					((Dissociation)selection[0]).addModulator();
				}else if(selection[0] instanceof ReversibleAssociation){
					ReversibleAssociation st=(ReversibleAssociation)selection[0];
					st.addModulator();
				}else if(selection[0] instanceof ReversibleStateTransition){
					ReversibleStateTransition st=(ReversibleStateTransition)selection[0];
					st.addModulator();
				}else{
					JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You have to select reaction not a molecule.","Warning",JOptionPane.WARNING_MESSAGE);
				}
			}else{
				JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You have to select JUST one reaction to which you wish to add a modulator.","Warning",JOptionPane.WARNING_MESSAGE);
			}
			
		}else{
			JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You have to select one reaction to which you wish to add a modulator.","Warning",JOptionPane.WARNING_MESSAGE);
		}
		
	}	
		
	public void undo() {
				
		try {
			undoManager.undo(graph.getGraphLayoutCache());
		} catch (Exception ex) {
			System.err.println(ex);
		} finally {
			updateHistoryButtons();
		}
	}
	
	public void redo() {
		
		try {
			undoManager.redo(graph.getGraphLayoutCache());
		} catch (Exception ex) {
			System.err.println(ex);
		} finally {
			updateHistoryButtons();
		}
	}
	
	public void zoomIn() {
		graph.setScale(1.2 * graph.getScale());
	}
	

	public void zoomOut() {
		graph.setScale(graph.getScale()/1.2);
	}

	public void remove() {
		if (!graph.isSelectionEmpty()) {
			Object[] cells = graph.getSelectionCells();
			boolean deleteReaction=false;
			cells = graph.getDescendants(cells);
			for(int i=0;i<cells.length;i++){
				if(GraphUtilities.isBiochamReaction(cells[i])){
					DefaultGraphCell c=(DefaultGraphCell)cells[i];
					BiochamEdgeData data=(BiochamEdgeData)c.getUserObject();
					String id=data.getName();
					((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).deleteRule(id,false,false,true);
					graph.deleteReaction(id,true);
					deleteReaction=true;
					break;
				}else if(GraphUtilities.isBiochamEntity(cells[i])){
						if(GraphUtilities.getBiochamEntityDataFromCell(cells[i]).getNumberOfConnections()==0 || GraphUtilities.getBiochamEntityDataFromCell(cells[i]).getInvolvedInReactions().size()==0){
							graph.getModel().remove(new Object[]{cells[i]});
						}/*else if(graph.getModel().getTarget(cells[i])==null || graph.getModel().getSource(cells[i])==null){
							graph.getModel().remove(graph.getDescendants(new Object[]{cells[i]}));//new Object[]{cells[i]});
						}*/else{
							if(JOptionPane.showConfirmDialog(BiochamMainFrame.frame," It is not recommended that you delete this molecule, because \n it's involved in a reaction. You have to delete the whole reaction \n in this case. Procceed in any case?\n\n","WARNING", JOptionPane.YES_NO_OPTION)==JOptionPane.YES_OPTION){
								graph.getModel().remove(graph.getDescendants(cells));
							}else{								
								break;
							}
						}
				}else if(cells[i] instanceof ESourceSink){
					if(JOptionPane.showConfirmDialog(BiochamMainFrame.frame," It is not recommended that you delete this molecule, because \n it's involved in a reaction. You have to delete the whole reaction \n in this case. Procceed in any case?\n\n","WARNING", JOptionPane.YES_NO_OPTION)==JOptionPane.YES_OPTION){
						graph.getModel().remove(graph.getDescendants(cells));
					}else{								
						break;
					}
				}else{					
					graph.getModel().remove(new Object[]{cells[i]});
					
				}
			}
		}
				
		/*for(int j=0;j<GraphUtilities.getAllCells(graph).length;j++){
			if(graph.getModel().getTarget(GraphUtilities.getAllCells(graph)[j])==null || graph.getModel().getSource(GraphUtilities.getAllCells(graph)[j])==null){
				graph.getModel().remove(new Object[]{GraphUtilities.getAllCells(graph)[j]});
			}
		}*/
			/*if(!deleteReaction){
				graph.getModel().remove(graph.getDescendants(cells));
			}*/
	}
	


	
	
	
	private JToolBar createToolbar(String s){
		JToolBar tb=new JToolBar(s);
		addSeparator(tb);		
		tb.setBackground(Utils.backgroundColor);	       
		tb.setFloatable(true);
		tb.setRollover(true);
		return tb;
	}
	
	private JButton createToolbarButton(ImageIcon icon,ImageIcon icon2,String actionCommand){
		JButton but=new JButton(icon);
		but.setBorderPainted(false);  
		but.setContentAreaFilled(false);  
		but.setFocusPainted(false);  
		but.setOpaque(false);  
		but.setRolloverEnabled(true);
        but.setRolloverIcon(icon2);
        but.setPressedIcon(icon);
		but.setActionCommand(actionCommand);
		but.addActionListener(this);
		but.setToolTipText(actionCommand);
		return but;
	}
	
//	 Update Undo/Redo Button State based on Undo Manager
	protected void updateHistoryButtons() {
		// The View Argument Defines the Context
		undo.setEnabled(undoManager.canUndo(graph.getGraphLayoutCache()));
		redo.setEnabled(undoManager.canRedo(graph.getGraphLayoutCache()));
		
	}


	


	public void valueChanged(GraphSelectionEvent e) {
		
		//Group Button only Enabled if more than One Cell Selected
		//group.setEnabled(graph.getSelectionCount() > 1);
		//Update Button States based on Current Selection
		
		boolean enabled = !graph.isSelectionEmpty();
		remove.setEnabled(enabled);				
		copy.setEnabled(enabled);
		cut.setEnabled(enabled);
		paste.setEnabled(enabled);
	}
	
	


	public void actionPerformed(ActionEvent e) {
		
		String cmd="";
		if(e.getSource() instanceof JButton){
			cmd=((JButton)e.getSource()).getActionCommand();
		}else if(e.getSource() instanceof JMenuItem){
			cmd=((JMenuItem)e.getSource()).getActionCommand();
		}
		if(cmd=="Copy"){				
				
				copyAction.actionPerformed(new ActionEvent(graph, e.getID(), e.getActionCommand(), e.getModifiers()));
				
		}else if(cmd=="Cut"){
				
				cutAction.actionPerformed(new ActionEvent(graph, e.getID(), e.getActionCommand(), e.getModifiers()));
				paste.setEnabled(true);
				
		}else if(cmd=="Paste"){
				
			try{
				pasteAction.actionPerformed(new ActionEvent(graph, e.getID(), e.getActionCommand(), e.getModifiers()));	
				
			}catch(Exception ew){}
				
		}else if(cmd=="Undo"){
			undo();
		}else if(cmd=="Redo"){
			redo();
		}else if(cmd=="Select All"){
			 selectAll();
		}else if(cmd=="Remove"){
			remove();
		}else if(cmd=="Clear"){			
			clearGraph();
		}else if(cmd=="Deselect All"){
			graph.clearSelection();
		}else if(cmd=="Zoom In"){
			zoomIn();
		}else if(cmd=="Zoom Out"){
			zoomOut();
		}else if(cmd=="Move To Front"){
			if (!graph.isSelectionEmpty())
				toFront(graph.getSelectionCells());
		}else if(cmd=="Move To Back"){
			if (!graph.isSelectionEmpty())
				toBack(graph.getSelectionCells());
		}/*else if(cmd=="Default Layout"){
			GraphLayouts.defaultLayout(graph);
		}*/else if(cmd=="Orthogonal Layout"){
			//GraphLayouts.ortogonalLayout(graph);
		}else if(cmd=="Hierarchic Layout"){
			//GraphLayouts.hierarchicLayout(graph);
		}else if(cmd=="Circular Layout"){
			//GraphLayouts.circularlayout(graph);
		}else if(cmd=="Tree Layout"){
			//GraphLayouts.treeLayout(graph);
		}else if(cmd=="Radial Layout"){
			//GraphLayouts.radialLayout(graph);
		}else if(cmd=="1"){
			//GraphLayouts.d1(graph);
		}else if(cmd=="2"){
			//GraphLayouts.d2(graph);
		}else if(cmd=="3"){
			//GraphLayouts.d3(graph);
		}else if(cmd=="4"){
			//GraphLayouts.d4(graph);
		}else if(cmd=="5"){
			//GraphLayouts.d5(graph);
		}else if(cmd=="6"){
			//GraphLayouts.d6(graph);
		}else if(cmd=="7"){
			//GraphLayouts.d7(graph);
		}else if(cmd.contains("DOT")){
			final String type=cmd;
			SwingWorker sw=new SwingWorker(){
				File tmp;
				@Override
				public Object construct() {
					
					
					try {
						tmp = File.createTempFile("dotGRAPH",".dot");
						if(type.contains("Top")){
							SbgnGraphToDot.save(graph,true,tmp,"TB");
						}else{
							SbgnGraphToDot.save(graph,true,tmp,"LR");
						}
						
						
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					//graph.saveAs();
					return null;
				}

				@Override
				public void finished() {
					
					//String cmd[] = {"ls", "/Applications/Jeux & Loisirs"};
					String output=tmp.getAbsolutePath().substring(0,tmp.getAbsolutePath().lastIndexOf(".dot"))+"LAYOUT.dot";
					String[] cmd = {"dot","-y","-Tdot",tmp.getAbsolutePath(),"-o", output};
					// String[] args = {DOT, "-Tgif", dot.getAbsolutePath(), "-o", img.getAbsolutePath()};
					Process dotProcess;
					
					try {
						dotProcess = Runtime.getRuntime().exec(cmd);
						try {
							dotProcess.waitFor();
						} catch (InterruptedException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
						File f=new File(output);
						if(f.exists()){
							Utils.debugMsg("*******"+"FIEL EXISTS!"+"*********");
							DotToSBGNGraph.read(f, graph,true);
						}
						tmp.deleteOnExit();	
						/*try {
							while(dotProcess.waitFor()!=0){
								
							}
							if(dotProcess.waitFor()==0){
								System.out.println("****"+dotProcess.getErrorStream().read()+"*****");
								File f=new File(tmp.getAbsolutePath().substring(0,tmp.getAbsolutePath().lastIndexOf(".dot")));
								InputStreamReader isr=new InputStreamReader(dotProcess.getInputStream());
								BufferedReader dotOutput = new BufferedReader(isr);
								OutputStream os=dotProcess.getOutputStream();
								PrintStream dotInput = new PrintStream(os,true);	
								if(f!=null){
									DotToSBGNGraph.read(f, graph);
								}
								tmp.deleteOnExit();	
							}else{
								System.out.println("FINISHES?=");
							}
						} catch (InterruptedException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}*/
						
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}		
					
					
				}};
				sw.start();
		}else if(cmd.contains("NEATO")){
			final String type=cmd;
			SwingWorker sw=new SwingWorker(){
				File tmp;
				@Override
				public Object construct() {
					
					
					try {
						tmp = File.createTempFile("dotGRAPH",".dot");
						if(type.contains("Top")){
							SbgnGraphToDot.save(graph,true,tmp,"TB");
						}else{
							SbgnGraphToDot.save(graph,true,tmp,"LR");
						}
						
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					//graph.saveAs();
					return null;
				}

				@Override
				public void finished() {
					
					//String cmd[] = {"ls", "/Applications/Jeux & Loisirs"};
					String output=tmp.getAbsolutePath().substring(0,tmp.getAbsolutePath().lastIndexOf(".dot"))+"LAYOUT.dot";
					String[] cmd = {"neato","-y","-Tdot",tmp.getAbsolutePath(),"-o", output};
					// String[] args = {DOT, "-Tgif", dot.getAbsolutePath(), "-o", img.getAbsolutePath()};
					Process dotProcess;
					
					try {
						dotProcess = Runtime.getRuntime().exec(cmd);
						try {
							dotProcess.waitFor();
						} catch (InterruptedException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
						File f=new File(output);
						if(f.exists()){
							Utils.debugMsg("*******"+"FIEL EXISTS!"+"*********");
							DotToSBGNGraph.read(f, graph,true);
						}
						tmp.deleteOnExit();	
						/*try {
							while(dotProcess.waitFor()!=0){
								
							}
							if(dotProcess.waitFor()==0){
								System.out.println("****"+dotProcess.getErrorStream().read()+"*****");
								File f=new File(tmp.getAbsolutePath().substring(0,tmp.getAbsolutePath().lastIndexOf(".dot")));
								InputStreamReader isr=new InputStreamReader(dotProcess.getInputStream());
								BufferedReader dotOutput = new BufferedReader(isr);
								OutputStream os=dotProcess.getOutputStream();
								PrintStream dotInput = new PrintStream(os,true);	
								if(f!=null){
									DotToSBGNGraph.read(f, graph);
								}
								tmp.deleteOnExit();	
							}else{
								System.out.println("FINISHES?=");
							}
						} catch (InterruptedException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}*/
						
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}		
					
					
				}};
				sw.start();
		
		}else if(cmd.contains("CIRCO")){
			final String type=cmd;
			SwingWorker sw=new SwingWorker(){
				File tmp;
				@Override
				public Object construct() {
					
					
					try {
						tmp = File.createTempFile("dotGRAPH",".dot");
						if(type.contains("Top")){
							SbgnGraphToDot.save(graph,true,tmp,"TB");
						}else{
							SbgnGraphToDot.save(graph,true,tmp,"LR");
						}
						
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					//graph.saveAs();
					return null;
				}

				@Override
				public void finished() {
					
					//String cmd[] = {"ls", "/Applications/Jeux & Loisirs"};
					String output=tmp.getAbsolutePath().substring(0,tmp.getAbsolutePath().lastIndexOf(".dot"))+"LAYOUT.dot";
					String[] cmd = {"circo","-y","-Tdot",tmp.getAbsolutePath(),"-o", output};
					// String[] args = {DOT, "-Tgif", dot.getAbsolutePath(), "-o", img.getAbsolutePath()};
					Process dotProcess;
					
					try {
						dotProcess = Runtime.getRuntime().exec(cmd);
						try {
							dotProcess.waitFor();
						} catch (InterruptedException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
						File f=new File(output);
						if(f.exists()){
							Utils.debugMsg("*******"+"FIEL EXISTS!"+"*********");
							DotToSBGNGraph.read(f, graph,true);
						}
						tmp.deleteOnExit();	
						/*try {
							while(dotProcess.waitFor()!=0){
								
							}
							if(dotProcess.waitFor()==0){
								System.out.println("****"+dotProcess.getErrorStream().read()+"*****");
								File f=new File(tmp.getAbsolutePath().substring(0,tmp.getAbsolutePath().lastIndexOf(".dot")));
								InputStreamReader isr=new InputStreamReader(dotProcess.getInputStream());
								BufferedReader dotOutput = new BufferedReader(isr);
								OutputStream os=dotProcess.getOutputStream();
								PrintStream dotInput = new PrintStream(os,true);	
								if(f!=null){
									DotToSBGNGraph.read(f, graph);
								}
								tmp.deleteOnExit();	
							}else{
								System.out.println("FINISHES?=");
							}
						} catch (InterruptedException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}*/
						
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}		
					
					
				}};
				sw.start();
		
		}else if(cmd.contains("Export")){
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,"image");			
					if (rep!=null) {				
						graph.exportAsImage(rep);
					}
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start();
			
		}
		else if(cmd.contains("Save")){
			
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					
					SbgnGraphToDot.save(graph,false,null,null);
					//graph.saveAs();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start();
			
		}else if(cmd.contains("Open")){
			
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					String f=Utils.showOpenDialog(BiochamMainFrame.frame,".dot");		
					if (f!=null && f!="") {
						DotToSBGNGraph.read(new File(f), graph,false);
					}
					
					/*SbgnGraphToDot.save(graph);*/
					//graph.saveAs();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start();
			
		}else if(cmd.contains("SBGN")){
			BrowserLauncher.openURL("http://sbgn.org/Documents/Specifications");
			
		}
	}


	

	private void hierarchicLayout() {
		// TODO Auto-generated method stub
		
	}









	private void treeLayout() {
		
		
	}









	private void circularlayout() {
		// TODO Auto-generated method stub
		
	}









	private void ortogonalLayout() {
		// TODO Auto-generated method stub
		
	}









	private void organicLayout() {
		// TODO Auto-generated method stub
		
	}









	public void clearGraph() {
		selectAll();
		remove();
	}

	private void selectAll() {
		graph.setSelectionCells(graph.getRoots());//cells.toArray());
	}

	
	
	
	
}
