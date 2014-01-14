package fr.inria.contraintes.biocham;

import fr.inria.contraintes.biocham.customComponents.BiochamFrame;
import fr.inria.contraintes.biocham.customComponents.CustomLayoutAKDock;
import fr.inria.contraintes.biocham.customComponents.DnDTabbedPane;
import fr.inria.contraintes.biocham.customComponents.CustomPlaf.MyPlafTabbedPane;
import fr.inria.contraintes.biocham.graphicalEditor.BiochamGraphEditorDesktop;
import fr.inria.contraintes.biocham.menus.BiochamMenuBar;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.Dimension;
import java.awt.Point;
import java.io.IOException;
import javax.swing.JMenuBar;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;




/**
 * Class thats represents the working area with Biocham models
 * 
 * @author Dragana Jovanovska  
 */ 
public class WorkbenchArea extends JSplitPane{

	
	
	private static final String screenName="Workbench";
	
	
	public static String app_path="";
	public static DnDTabbedPane tabbedPane, tabbedPane_me, tabbedPane_rge;	
	public static BiochamDynamicTree tree;
	public static WorkbenchToolBars wtb;
	public static final CustomLayoutAKDock toolBarLayout = new CustomLayoutAKDock();
	private JToolBar toolBar;
	public static BiochamMenuBar biochamMenuBar;
	private JMenuBar mb;
	BiochamModel model;

	
	

	   
   /** Creates the content of main screen in the Workbench: the workbench tree explorer, the toolbar, the menubar etc..
    * 
    */
	public WorkbenchArea(){
		
	
		super();
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		
		app_path="";
		
		tabbedPane = new DnDTabbedPane();
		tabbedPane.setUI(new MyPlafTabbedPane());
		tabbedPane.setOpaque(true);
		tabbedPane.setName("tabbedPane");
		tabbedPane.setBackground(Utils.backgroundColor);
		tabbedPane.setFocusable(false);
		tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);     
		biochamMenuBar=new BiochamMenuBar();
		tree=new BiochamDynamicTree(this);     
		
		mb=biochamMenuBar.createMenuBar(tree);	                
		super.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		super.setLeftComponent(tree);
		super.setRightComponent(tabbedPane);		
		setResizeWeight(0.1);  
		setDividerLocation(400);
		wtb=new WorkbenchToolBars(tree);
		wtb.setIntro(true);
		toolBar=wtb.getFirstToolBar();
	   
	}
	
	public void resetDividerLocation(){
		setDividerLocation(400);
	}
	public void setMenubarState(boolean b){
		for(int i=0;i<6;i++){
			mb.getComponent(i+1).setEnabled(b);
		}
		mb.revalidate();
	}
	
	public BiochamFrame replaceTabbedPane(BiochamModel m){
		
	
		if(m.getGraphEditor()==null){
			BiochamGraphEditorDesktop desktop=new BiochamGraphEditorDesktop(m);
			m.setGraphEditor(desktop);
			
			
		}
		if(m.getGraphEditorFrame()==null){
			BiochamFrame d=new BiochamFrame("Reaction Graph Editor");
			d.getContentPane().add(new JScrollPane(m.getGraphEditor()));
			Point pos = BiochamMainFrame.frame.getLocationOnScreen();
			d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
		    d.setResizable(true);
		    d.setSize(new Dimension(480, 580));	    
		    d.setLocationRelativeTo(BiochamMainFrame.frame);
		    d.setAlwaysOnTop(false);
		    d.setFocusable(true);		    
		    d.setVisible(true);	   
		    m.setGraphEditorFrame(d);			
			model=m;
			setDividerLocation(400);
			pos=null;
		}
		m.getGraphEditorFrame().toFront();
		return m.getGraphEditorFrame();
		
	}
	
	public void setModelEditor(BiochamModel m){
		super.setRightComponent(m.getModelEditor());
		setDividerLocation(400);
		model=m;
	}
	public void setAbstractionsEditor(BiochamModel m){		
		super.setRightComponent(m.getAbstractionView());
		setDividerLocation(400);
		model=m;
		BiochamDynamicTree.currentView="Abstractions";	
		
		DefaultMutableTreeNode parent=(DefaultMutableTreeNode)BiochamDynamicTree.workbench.tree.getSelectedNode().getParent();
		TreePath pathToLeaf=null;
		if(parent.getUserObject() instanceof String && parent.getUserObject().toString().equals("Biocham Models")){
			
			parent=(DefaultMutableTreeNode)BiochamDynamicTree.workbench.tree.getSelectedNode();
			pathToLeaf = new TreePath(((DefaultMutableTreeNode)parent.getChildAt(4)).getPath());
		}else if(parent.getUserObject() instanceof BiochamModel){
			pathToLeaf = new TreePath(((DefaultMutableTreeNode)parent.getChildAt(4)).getPath());
		}else if(parent.getUserObject() instanceof BiochamDynamicTreeSeparator){
			parent=(DefaultMutableTreeNode)parent.getParent();
			pathToLeaf = new TreePath(((DefaultMutableTreeNode)parent.getChildAt(4)).getPath());
		}
		BiochamDynamicTree.workbench.tree.tree.setSelectionPath(pathToLeaf);
		
	}
	
	public void setSimulationsPanel(BiochamModel m){

		super.setRightComponent(m.getSimulationsPanel());
		setDividerLocation(400);	
		model=m;
		DefaultMutableTreeNode parent=(DefaultMutableTreeNode)BiochamDynamicTree.workbench.tree.getSelectedNode().getParent();
		TreePath pathToLeaf=null;
		if(parent.getUserObject() instanceof String && parent.getUserObject().toString().equals("Biocham Models")){
			
			parent=(DefaultMutableTreeNode)BiochamDynamicTree.workbench.tree.getSelectedNode();
			pathToLeaf = new TreePath(((DefaultMutableTreeNode)parent.getChildAt(1)).getPath());
		}else if(parent.getUserObject() instanceof BiochamModel){
			pathToLeaf = new TreePath(((DefaultMutableTreeNode)parent.getChildAt(1)).getPath());
		}else if(parent.getUserObject() instanceof BiochamDynamicTreeSeparator){
			parent=(DefaultMutableTreeNode)parent.getParent();
			pathToLeaf = new TreePath(((DefaultMutableTreeNode)parent.getChildAt(1)).getPath());
		}
		BiochamDynamicTree.workbench.tree.tree.setSelectionPath(pathToLeaf);
		
	}
	
	public void setModelEditor(BiochamModel m,boolean firstStart){
		setModelEditor(m);
		if(firstStart){
			m.getModelEditor().setDividerLocation(700);
		}
				
	}
	public void setGraphEditor(BiochamModel m){
		super.setRightComponent(m.getGraphEditor());
		setDividerLocation(400);
		model=m;
	}
	public void returnTabbedPane(){		
		super.setRightComponent(tabbedPane);	
		setDividerLocation(400);
	}
	
		
	
	public void showWorkbenchTabbedPane(){
		super.setRightComponent(tabbedPane);
		setDividerLocation(400);
	}
	
			
	public JMenuBar getMenuBar(){
		return mb;		
	}
	public JToolBar getToolBar(){
		return toolBar;
	}
		
	public String toString(){
		return screenName;
	}
	
	/**
	 * Removes used references to unneeded objects
	 */
	public static void disposeWorkbench() throws IOException{
		   
		   
	}

	
}
