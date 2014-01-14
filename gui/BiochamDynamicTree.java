package fr.inria.contraintes.biocham;

import fr.inria.contraintes.biocham.commandLine.SimpleCommandLine;
import fr.inria.contraintes.biocham.customComponents.BiochamFrame;
import fr.inria.contraintes.biocham.customComponents.PDFViewer;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.documentation.BiochamDocumentation;
import fr.inria.contraintes.biocham.documentation.TutorialArticle;
import fr.inria.contraintes.biocham.menus.BiochamMenuBar;
import fr.inria.contraintes.biocham.menus.ElementMenu;
import fr.inria.contraintes.biocham.menus.ElementMenuModel;
import fr.inria.contraintes.biocham.modelData.AbstractionView;
import fr.inria.contraintes.biocham.modelData.SimulationView;
import fr.inria.contraintes.biocham.modelData.ParamTableCTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableLTLSpecifications;
import fr.inria.contraintes.biocham.utils.BrowserLauncher;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.io.File;
import java.io.IOException;
import java.io.FilenameFilter;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Arrays;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTree;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLFrameHyperlinkEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;


/**
 * 
 * A class representing the working explorer for the opened Biocham Models.
 * It implements a TreeSelectionListener..  
 * 
 * @author Dragana Jovanovska 
 * 
 */

public class BiochamDynamicTree extends JPanel implements TreeSelectionListener{
			  
	
	/**
	 * Static fields common for all the graphical interface.
	 * 
	 * */
	public static WorkbenchArea workbench;   
        public static HashMap<Integer,BiochamModel> openedModels;
        public static HashMap<Integer,DefaultMutableTreeNode> openedNodes;
	public static ArrayList<ImageIcon> jplots;
	public static BiochamModel currentModel;
	public static int nodeIndex=0;
	public static int newNodeSuffix = 0;
	public static JPanel comparizonWindow;
	public static BiochamFrame comparizonFrame;
	public static boolean compWindowAlreadyOpened=false;	
	public static boolean firstLoad=true;
	public static String currentView="";  
	public static ElementMenu modelMenu;	
        public static BiochamDynamicTreeActionListener treeListener;
    
    /**
     * 
     * There can be at least 3 different type of instances of this class:
     * 	- one for the model explorer tree.
     *  - one for the documentation explorer tree.
     *  - one for each reaction rule.
     *  
     * */
    protected DefaultMutableTreeNode rootNode;
    protected DefaultTreeModel treeModel;
    public JTree tree;    
    private UniversalScrollPane scrollPane;    
    
    private final static String[] EXCLUDE_FILES = {"manual.pdf"};
	
	
    
    //3 constructors for 3 different purposes: BiochamModels Explorer tree, ExpandedRules tree and Documentation Explorer tree.     
    
    
    /**
     * It creates the main tree explorer for Biocham Models.
     * 
     * First the Splash is created, after the BiochamMainFrame which creates its children(the Workbench and the Documentation).
     * The Workbench creates the model's explorer tree, i.e. it calls this BiochamDynamicTree constructor.
     * 
     */
    public BiochamDynamicTree(WorkbenchArea workingArea){
    	
		super(new GridLayout(1,0));	
		treeListener=new BiochamDynamicTreeActionListener();
		initializeTree("Biocham Models");       	
	 	tree.setCellRenderer(new BiochamDynamicTreeRenderer());		
		openedModels=new HashMap<Integer,BiochamModel>();
		openedNodes=new HashMap<Integer,DefaultMutableTreeNode>();
		jplots=new ArrayList<ImageIcon>();       

        comparizonWindow = new JPanel();     
        comparizonWindow.setBackground(Utils.backgroundColor);    
        comparizonFrame=new BiochamFrame("Simulation Plots Comparison");
       	workbench=workingArea;
       	BiochamMainFrame.tree=this;      
       	modelMenu=new ElementMenuModel(this);      
	} 
    
    
    
	 /**
     * It creates the main tree explorer of Documentation.   
     * 
     */
    public BiochamDynamicTree(BiochamDocumentation tutorials){
      
      super(new GridLayout(1,0));         
      initializeTree("Documentation");    
       tree.setCellRenderer(new BiochamDynamicTreeRenderer());
         File[] files;
         String dir=Utils.DEPLOYMENT_DIR;  
         File docPath;
          if(dir!=null){
            try {
              docPath = new File(dir).getParentFile();
              docPath = new File(docPath.getPath(), "DOC");
              //dir=dir+File.separator+"DOC"+File.separator;
              Utils.debugMsg("DOC path="+docPath+"\n");
              files = docPath.listFiles(new FilenameFilter() {
                public boolean accept(File dir, String name) {
                  boolean fileAccepted = name.toLowerCase().matches(".*\\.(pdf|html)$")
                    && ! Arrays.asList(EXCLUDE_FILES).contains(name.toLowerCase());
                  return (fileAccepted);
                }
              });
              
              if (files.length > 0) {
                for (File f : files) {
                  addNodeObject(rootNode,new TutorialArticle(f.getName(),f.getPath()));
                }
              }
            }
            catch(Exception e){
              e.printStackTrace();
            }
          }
    }
            
   
    
    /**
     * It creates each biocham reaction rule as a tree. The root is the rule,and its children are the expanded rules. 
     * 
     */
    public BiochamDynamicTree(String parentRule){
    	
    	super(new GridLayout(1,0));	    	
    	initializeTree(parentRule);    	
       	((javax.swing.plaf.basic.BasicTreeUI) tree.getUI()).setExpandedIcon(Icons.icons.get("85.png"));
       	((javax.swing.plaf.basic.BasicTreeUI) tree.getUI()).setCollapsedIcon(Icons.icons.get("81.png"));
        tree.setToolTipText("Show expanded rules...");
       	tree.setBackground(Utils.backgroundColor);
       	
       //	tree.setCellRenderer(new BiochamDynamicTreeRenderer("rules"));    	
    }
    
    
    
    /**
     * It initializes the tree, and adds it to a container.
     * 
     */
	private void initializeTree(String treeTitle) {
		
		rootNode = new DefaultMutableTreeNode(treeTitle);      
       	treeModel = new DefaultTreeModel(rootNode);	
       	tree = new JTree(treeModel); 
       	tree.setName(treeTitle);
    	tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
       	tree.setShowsRootHandles(true);
       	tree.setSelectionRow(0);             
       	tree.setRowHeight(0);       	
       	tree.addTreeSelectionListener(this);       
       	UniversalScrollPane scrollPane = new UniversalScrollPane(tree);  	       	
       	setTreeView(scrollPane);       	
       	add(scrollPane);       
	}
    
   
	
	
	
    
 
    
    /**
	 * Adds a node in the model's explorer tree for the command line interface.
	 * Called by the BiochamCommandLine class.
	 */
    public void addCommandLineNodeToTree(DefaultMutableTreeNode p, SimpleCommandLine line) {    	
    	addNodeObject(p,line);
    	treeModel.reload();		
	}
    
    
    
	/**
	 * Adds a node in the model's explorer tree for the warnings.
	 * Called by the BiochamWarnings class.
	 */
	public void addWarningsNodeToTree(DefaultMutableTreeNode lastChild,BiochamWarnings warnings){
		addNodeObject(lastChild,warnings);
		treeModel.reload();		
	}	
	
	
	
	
	/**
	 * Adds a rule node to the Biocham Rules tree on the RulesView panel.
	 * @param child The expanded rule child that has to be added to its parent node
	 */
	public DefaultMutableTreeNode addRuleObject(Object child) {	        
				
	    	DefaultMutableTreeNode parentNode = null;
	        TreePath parentPath = tree.getSelectionPath();
	        if (parentPath == null) {
	            parentNode = rootNode;
	        } else {
	            parentNode = (DefaultMutableTreeNode)(parentPath.getLastPathComponent());
	        }
	        DefaultMutableTreeNode addedObject=addNodeObject(parentNode, child, true);	 	        
	        parentNode=null;
	        parentPath=null;      
	        return addedObject;
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
            parent = rootNode;
        }       
        treeModel.insertNodeInto(childNode, parent,parent.getChildCount());	
        if (shouldBeVisible) {		
        	TreePath path=new TreePath(childNode.getPath());
            tree.scrollPathToVisible(path);
        	path=null;
        }
        return childNode;
    }
    
    
    
    
    
    
    
    /**
     * Returns the currently selected node in the model's explorer tree.
     * 
     * */
    public DefaultMutableTreeNode getSelectedNode(){
    	 //get the selected node
        return  (DefaultMutableTreeNode)tree.getLastSelectedPathComponent();  
    }
    
    
    
    
    /**
     * Makes the current view focus on reaction Model Editor.
     * 
     * */
	public static void setFocusOnMEditor(){
							
		BiochamDynamicTree.workbench.setModelEditor(currentModel,true);	
		//currentModel.refreshAllViews();	    	
		currentView="modelEditor";	
	}
	 
  
	
	
	
	
	
	
	////////////////////////  I M P O R T A N T  ////////////////////////
    
    /**
	 * Generates action depending of the currently selected node in the Biocham Models' Explorer tree
	 */	  
	public void valueChanged(TreeSelectionEvent e) {
			
			DefaultMutableTreeNode node=(DefaultMutableTreeNode) tree.getLastSelectedPathComponent();
			if(node==null) return;
			Object nodeInfo=node.getUserObject();
			
			if(tree.getName().contains("Biocham Models")){
							
				/*if(workbench!=null && workbench.getRightComponent()!=null){
					workbench.tabbedPane.removeAll();
				}
				if(openedModels.size()>0){
					BiochamMenuBar.setMenusEnabled(true);					
					workbench.wtb.setOpened(true);
					workbench.wtb.setSaved(true);
					workbench.wtb.setIntro(true);
				}else{
					BiochamMenuBar.setMenusEnabled(false);					
					workbench.wtb.setOpened(false);
					workbench.wtb.setSaved(false);
					workbench.wtb.setIntro(true);
				}*/
				if(nodeInfo instanceof BiochamModelElement){ //CTL/LTL or ReactionModelEditor!!!!!
					
				    BiochamModelElement element=(BiochamModelElement)nodeInfo; 	
				    currentModel=element.getModel();
				    Utils.debugMsg("\n\n\n\nvalueChanged: Set CurrentModel="+currentModel.getModelName()+"\n\n\n\n");
				    BiochamMenuBar.refreshMenusContext(currentModel);
				    /*SwingWorker sw=new SwingWorker(){
						@Override
						public Object construct() {
							   BiochamMenuBar.refreshMenusContext(currentModel);
							return null;
						}
						@Override
						public void finished() {}				    	
				    };
				    sw.start();
				 
				    if(element.getModel().isSaved()){			    	
		            	  WorkbenchArea.wtb.setOpened(true);	
		            	  WorkbenchArea.wtb.setSaved(true);
				    }else{			    	
				    	  WorkbenchArea.wtb.setSaved(false);
		            	  WorkbenchArea.wtb.setOpened(true);		            	  
				    }	*/	    			    
				   // JPanel panel=element.getParametersPanel();	
				   //Component[] comps=panel.getComponents();
	    			BiochamModel model=(BiochamModel) ((DefaultMutableTreeNode) node.getParent()).getUserObject(); 
					model.setCommandLineMode(false);			
					
	    			if(element.getElementName().equals("Reaction Rule Model")){
	    				currentModel=model;
	    				setFocusOnMEditor();				
	    			}else{							
	    				BiochamDynamicTree.workbench.setRightComponent(WorkbenchArea.tabbedPane);	    				
					
	    				if(element.getElementName().contains("Boolean Temporal")){
	    					currentView="Boolean Temporal Properties";	
	    					WorkbenchArea.tabbedPane.removeAll();
							WorkbenchArea.tabbedPane.add("Boolean Temporal Properties",((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).getView());		 
	    				}else{
	    					WorkbenchArea.tabbedPane.removeAll();
	    					currentView="Numerical Temporal Properties";
	    					WorkbenchArea.tabbedPane.add("Numerical Temporal Properties",((ParamTableLTLSpecifications)model.getLtlSpecifications().getParamTable()).getView());
							//WorkbenchArea.tabbedPane.add("Numerical Temporal Properties",(LTLView)nodeInfo);
	    					//showNumericalTemporalProperties();
	    				}
	    				BiochamDynamicTree.workbench.setDividerLocation(400);
	    				WorkbenchArea.tabbedPane.setSelectedIndex(WorkbenchArea.tabbedPane.getTabCount()-1);
						
	    				/*System.out.println("UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU2222222");
	    				showTemporalPropertiesPanel(panel, comps, model, panel.getName());  */
	    				//currentView=panel.getName();showTemporalPropertiesPanel(panel, comps, model, panel.getName());
	    			}		    			
	    			//panel=null;    		
	    			element=null;
	    			model=null;
	    			
				}else if(nodeInfo instanceof AbstractionView){
					currentView="Abstractions";					
					currentModel=((AbstractionView)nodeInfo).getBiochamModel();	
					BiochamMenuBar.refreshMenusContext(currentModel);
					BiochamDynamicTree.workbench.setAbstractionsEditor(currentModel);
					
				}else if(nodeInfo instanceof SimulationView){
					currentView="Simulations";
					currentModel=((SimulationView)nodeInfo).getModel();	
					BiochamMenuBar.refreshMenusContext(currentModel);
					WorkbenchArea.tabbedPane.removeAll();
					BiochamDynamicTree.workbench.setSimulationsPanel(currentModel);//.setRightComponent(WorkbenchArea.tabbedPane);
					//WorkbenchArea.tabbedPane.add("Simulations",((ParamTableSimulations)nodeInfo).getModel().getSimulationsScrollPane());					
					//WorkbenchArea.tabbedPane.setSelectedIndex(WorkbenchArea.tabbedPane.getTabCount()-1);
					
					
				}else if(nodeInfo instanceof BiochamModel || (nodeInfo.toString().indexOf("Biocham Models") >= 0) ){
			
					if(nodeInfo instanceof BiochamModel){
						BiochamModel m=(BiochamModel)nodeInfo;
						
						if(m.isExistingModel()){							
							WorkbenchArea.tabbedPane.removeAll();
							BiochamDynamicTree.workbench.setRightComponent(WorkbenchArea.tabbedPane);
							m.setInitFileModelTab();	
							currentModel=m;							
						}else{
							WorkbenchArea.tabbedPane.removeAll();
							BiochamDynamicTree.workbench.setRightComponent(WorkbenchArea.tabbedPane);
							currentModel=m;		
						}
						BiochamMenuBar.refreshMenusContext(currentModel);
						m=null;	
						currentView="Model";	 
					}
					
					if(!(nodeInfo instanceof BiochamModel)){	
						WorkbenchArea.wtb.setSaved(false);
						WorkbenchArea.wtb.setOpened(false);					
						BiochamMenuBar.menuBar.revalidate();
						currentView=null;
					}else{					
						BiochamModel m=(BiochamModel)nodeInfo;
						m.setCommandLineMode(false);
						currentModel=m;							
						Utils.debugMsg("\n\n\n\nvalueChanged: Set CurrentModel="+currentModel.getModelName()+"\n\n\n\n");
						BiochamMenuBar.refreshMenusContext(currentModel);
						m=null;
					}	
					
				}
				else if(nodeInfo instanceof BiochamWarnings){		
					
					 currentView="BiochamWarnings";
					 BiochamWarnings warnings=(BiochamWarnings)nodeInfo; 	
					 BiochamModel model=warnings.getModel();
					 model.setCommandLineMode(false);
					 currentModel=model;	
					 Utils.debugMsg("\n\n\n\nvalueChanged: Set CurrentModel="+currentModel.getModelName()+"\n\n\n\n");
					 BiochamMenuBar.refreshMenusContext(currentModel);
					 warnings.getTarea().repaint();
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
					 BiochamDynamicTree.workbench.setRightComponent(WorkbenchArea.tabbedPane);	
					 BiochamDynamicTree.workbench.setDividerLocation(400);
					 WorkbenchArea.tabbedPane.add("BiochamWarnings and Errors",new UniversalScrollPane(model.getWarningsPanel()));					
	                 WorkbenchArea.tabbedPane.setSelectedIndex(WorkbenchArea.tabbedPane.getTabCount()-1);
	                 warnings=null;
	                 model=null;
	                 
				}else if(nodeInfo instanceof SimpleCommandLine){
					
					 currentView="BiochamCommandLine";
					 SimpleCommandLine cmdLine=(SimpleCommandLine)nodeInfo; 	
					 BiochamModel model=cmdLine.getModel();
					 model.setCommandLineMode(false);
					 currentModel=model;
					 BiochamMenuBar.refreshMenusContext(currentModel);
					 int tabs=WorkbenchArea.tabbedPane.getTabCount();
					 boolean nth=true;
					 for(int i=0;i<tabs;i++){
				    	if(WorkbenchArea.tabbedPane.getTitleAt(i).equals("Biocham Command-Line")){
				    		WorkbenchArea.tabbedPane.removeTabAt(i);			    		
				    		break;
				    	}
				     }
				     if(nth){
						
						 currentModel=model;
						 Utils.debugMsg("\n\n\n\nvalueChanged: Set CurrentModel="+currentModel.getModelName()+"\n\n\n\n");
						 BiochamMenuBar.refreshMenusContext(currentModel);				
						 int cnt=0;
						 tabs=WorkbenchArea.tabbedPane.getTabCount();
						 for(int i=0;i<tabs;i++){						 
							 String name=WorkbenchArea.tabbedPane.getTitleAt(cnt);
							 if(!name.contains("Cmaes") && !name.contains("ODE-") && !name.contains("Boolean-") && !name.contains("Stochastic-") && !name.contains("Reaction Graph") && !name.contains("Influences Graph")){
								 WorkbenchArea.tabbedPane.remove(cnt);							
							 }else{
								 cnt++;
							 }
							 name=null;
						 }
						 BiochamDynamicTree.workbench.setRightComponent(WorkbenchArea.tabbedPane);
						 BiochamDynamicTree.workbench.setDividerLocation(400);
						 WorkbenchArea.tabbedPane.add("Biocham Command-Line",cmdLine);
	                     WorkbenchArea.tabbedPane.setSelectedIndex(WorkbenchArea.tabbedPane.getTabCount()-1);                 
				     }	
				     cmdLine=null;
				     model=null;
				     
				}
				
			}else if(tree.getName().contains("Documentation")){
				if(nodeInfo instanceof TutorialArticle){
					
					final TutorialArticle ta=(TutorialArticle)nodeInfo;
					final String fName=ta.getFileName();
					if(BiochamDocumentation.tabbedPane.getTabCount()>0){
						BiochamDocumentation.tabbedPane.removeAll();
					}				
					if(fName.endsWith(".pdf")){
						SwingWorker sw=new SwingWorker(){
							
							PDFViewer pdf=new PDFViewer(true);
							@Override
							public Object construct() {
								pdf.doOpen(fName);							
								return null;
							}

							@Override
							public void finished() {
								BiochamDocumentation.tabbedPane.add(ta.toString(),pdf);
								BiochamDocumentation.tabbedPane.setSelectedIndex(BiochamDocumentation.tabbedPane.getTabCount()-1);
								
							}};
						sw.start();
						
						
					}else if(fName.endsWith(".html")){
											
						int answer=JOptionPane.showConfirmDialog(BiochamMainFrame.frame, "Loading may be slow. Do you want to open it in a Internet Browser?","Choice",JOptionPane.YES_NO_OPTION);
						if(answer==JOptionPane.NO_OPTION){
							
							final JEditorPane ep=new JEditorPane();	
							File file = null;
							
							ep.setEditable(false);	
							String dir=Utils.DEPLOYMENT_DIR;			
							if(dir!=null){
                                                          try {
                                                            File docPath = new File(dir).getParentFile();
                                                            file = new File(fName);
                                                            ep.setPage(file.toURL());
                                                            
                                                                ep.setName(file.getAbsolutePath());
                                                                ep.addHyperlinkListener(new HyperlinkListener(){
                
                                                                        public void hyperlinkUpdate(HyperlinkEvent e) {
                                                                                if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
                                                                                JEditorPane pane = (JEditorPane) e.getSource();
                                                                                if (e instanceof HTMLFrameHyperlinkEvent) {
                                                                                HTMLFrameHyperlinkEvent evt = (HTMLFrameHyperlinkEvent)e;
                                                                                HTMLDocument doc = (HTMLDocument)pane.getDocument();
                                                                                doc.processHTMLFrameHyperlinkEvent(evt);
                                                                                } else {
                                                                                URL url = e.getURL();
                                                                                //Logging.cat.debug("URL: " url " desc: " e.getDescription() " element: " + e.getSourceElement());
                
                                                                                if (url == null && e.getDescription().startsWith("#")){                                                         
                                                                                        pane.scrollToReference(e.getDescription().substring(1));
                                                                                return;
                                                                                }
                
                                                                                try {
                                                                                        pane.setPage(url);
                                                                                } catch (IOException e1) {
                                                                                        try {
                                                                                                JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Couldn't go to the requested page.","Warning",JOptionPane.WARNING_MESSAGE);
                                                                                                pane.setPage((new File(pane.getName())).toURL());                                                                               
                                                                                        } catch (IOException e2) {
                                                                                                // TODO Auto-generated catch block
                                                                                                e2.printStackTrace();
                                                                                        }
                                                                                }
                                                                                }
                                                                                }
                                                                        }});
                                                        
                                                        BiochamDocumentation.tabbedPane.add(ta.toString(),new UniversalScrollPane(ep));// shows the latest drawn plot...
                                                        BiochamDocumentation.tabbedPane.setSelectedIndex(BiochamDocumentation.tabbedPane.getTabCount()-1);
                                                            
                                                          }
                                                          catch(Exception e3){
                                                            e3.printStackTrace();
                                                          }
							}
						}else{
							
							SwingWorker sw=new SwingWorker(){

								@Override
								public Object construct() {								
									BrowserLauncher.openURL(fName);
									return null;
								}

								@Override
								public void finished() {
									// TODO Auto-generated method stub
									
								}};
							sw.start();
							
						}
					}				
				}
			}
	}

	

	
	/**
	 * Closes all opened popups from the GUI.
	 * 
	 * */
	public static void disposeAllOpenedFrames(){
		if(BiochamDynamicTree.currentModel!=null){
			for(int i=0;i<BiochamDynamicTree.currentModel.getPopupFrames().length;i++){
				if(BiochamDynamicTree.currentModel.getPopupFrames()[i]!=null){
					BiochamFrame f=BiochamDynamicTree.currentModel.getPopupFrames()[i];				
					f.setVisible(false);							
					f.dispose();	
				}
			}
		}				
	}

	
	
	
	
	
	
	/**
	 *  TD!
	 * */
	public static void showBooleanTemporalProperties(){
		
		/*JPanel panel=BiochamDynamicTree.currentModel.getLtlSpecifications().getParametersPanel();
		Component[] comps=panel.getComponents();
		ArrayList<Component> cs=new ArrayList<Component>();
		for(int i=0;i<comps.length;i++){
			if((comps[i] instanceof JButton)){
				if(!((JButton)comps[i]).getActionCommand().equals("addCTL") && !((JButton)comps[i]).getActionCommand().equals("addGenCTL")
						&& !((JButton)comps[i]).getActionCommand().equals("checkCTL") && !((JButton)comps[i]).getActionCommand().equals("genCTL") && !((JButton)comps[i]).getActionCommand().equals("nusmv")
						&& !((JButton)comps[i]).getActionCommand().equals("reduceModel")&& !((JButton)comps[i]).getActionCommand().equals("learnRules") 
						&& !((JButton)comps[i]).getActionCommand().equals("reviseModel")){
					
					cs.add(comps[i]);
				}    													
			}else if(!(comps[i] instanceof JLabel)){
				cs.add(comps[i]);
			}
		}
		panel.removeAll();
		panel=((ParamTableCTLSpecifications)BiochamDynamicTree.currentModel.getCtlSpecifications().getParamTable()).refreshPanel(cs);	
		cs.clear();
		cs=null;
		int numComps1=panel.getComponentCount();
		int dynHeight1=(numComps1/2)*40;
		JPanel p11=((ParamTableCTLSpecifications)BiochamDynamicTree.currentModel.getCtlSpecifications().getParamTable()).getCTLModelChecking();
		panel.setPreferredSize(new Dimension(700,dynHeight1+200));
		UniversalScrollPane ctlSPEC=new UniversalScrollPane(panel);
		JPanel ctlCmds=((ParamTableCTLSpecifications)BiochamDynamicTree.currentModel.getCtlSpecifications().getParamTable()).getCommandsPanel();
		ctlCmds.setPreferredSize(new Dimension(200,270));
		UniversalScrollPane ctlCommands=new UniversalScrollPane(ctlCmds);
		ctlCmds.revalidate();
		UniversalScrollPane ctlresults=new UniversalScrollPane(p11);
		JSplitPane upCTL=new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,ctlCommands,ctlSPEC);		
		upCTL.setResizeWeight(0.2);
		upCTL.setDividerLocation(0.5);
		upCTL.setDividerSize(1);
		upCTL.setContinuousLayout(true);
		JSplitPane spCTL=new JSplitPane(JSplitPane.VERTICAL_SPLIT,upCTL,ctlresults);			
		spCTL.setResizeWeight(0.5);			
		spCTL.setDividerLocation(0.5);
		WorkbenchArea.tabbedPane.removeAll();*/
		/*CTLView v=new CTLView(((ParamTableCTLSpecifications)BiochamDynamicTree.currentModel.getCtlSpecifications().getParamTable()).getCtlModel());
		//new JScrollPane(v);
		WorkbenchArea.tabbedPane.add("Boolean Temporal Properties",v);		
		BiochamDynamicTree.workbench.resetDividerLocation();
		WorkbenchArea.tabbedPane.setSelectedIndex(WorkbenchArea.tabbedPane.getTabCount()-1);*/
	}
	
	
	
	/**
	 *  TD!
	 * */
	public static void showNumericalTemporalProperties(){
		
		ArrayList<Component> cs2=new ArrayList<Component>();
		JPanel panelLTLSpecs=BiochamDynamicTree.currentModel.getLtlSpecifications().getParametersPanel();
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
		panelLTLSpecs.removeAll();			
		panelLTLSpecs=((ParamTableLTLSpecifications)BiochamDynamicTree.currentModel.getLtlSpecifications().getParamTable()).refreshPanel(cs2);			
		cs2.clear();			
		cs2=null;
		int numComps2=panelLTLSpecs.getComponentCount();
		int dynHeight2=(numComps2/2)*40;						
		JPanel p22=((ParamTableLTLSpecifications)BiochamDynamicTree.currentModel.getLtlSpecifications().getParamTable()).getLTLModelChecking();			
		panelLTLSpecs.setPreferredSize(new Dimension(700,dynHeight2+200));
		UniversalScrollPane ltlSPEC=new UniversalScrollPane(panelLTLSpecs);			
		JPanel ltlCmds=((ParamTableLTLSpecifications)BiochamDynamicTree.currentModel.getLtlSpecifications().getParamTable()).getCommandsPanel();			
		ltlCmds.setPreferredSize(new Dimension(250,350));			
		UniversalScrollPane ltlCommands=new UniversalScrollPane(ltlCmds);			
		ltlCmds.revalidate();		
		UniversalScrollPane ltlResults=new UniversalScrollPane(p22);
		JSplitPane upLTL=new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,ltlCommands,ltlSPEC);
		upLTL.setResizeWeight(0.3);
		upLTL.setDividerLocation(0.5);
		upLTL.setDividerSize(1);
		upLTL.setContinuousLayout(true);
		JSplitPane spLTL=new JSplitPane(JSplitPane.VERTICAL_SPLIT,upLTL,ltlResults);
		spLTL.setResizeWeight(0.5);
		spLTL.setDividerLocation(0.5);
		WorkbenchArea.tabbedPane.removeAll();
		WorkbenchArea.tabbedPane.add("Numerical Temporal Properties",spLTL);
		BiochamDynamicTree.workbench.resetDividerLocation();
		WorkbenchArea.tabbedPane.setSelectedIndex(WorkbenchArea.tabbedPane.getTabCount()-1);
	
	}
	
	/**
	 * TD!
	 * */
	public static void showTemporalPropertiesPanel(JPanel panel, Component[] comps, BiochamModel model, String elementName) {
		
	
		if(elementName.equals("Boolean Temporal Properties")){
			showBooleanTemporalProperties();		
		}else if(elementName.equals("Numerical Temporal Properties")){
			showNumericalTemporalProperties();			
		}
		
	}
	
	
	
	
	
	
	
	
	
	
	
	
	/******* Some Getters and Setters *******/
		
	
	public static void addToOpenedModels(int index,BiochamModel model){
    	openedModels.put(index, model);
    } 
	public static BiochamModel getModel(int index){
    	return openedModels.get(index);
    } 
    public static void addToOpenedNodes(int index,DefaultMutableTreeNode node){
    	openedNodes.put(index, node);
    }
    
    public static WorkbenchArea getWorkbench() {
		return workbench;
	}
	public static MutableTreeNode getNode(int index){
		return openedNodes.get(index);
	}	 
	
	public UniversalScrollPane getTreeView() {
		return scrollPane;
	}
	public void setTreeView(UniversalScrollPane treeView) {
			scrollPane = treeView;
	}    
    
	public ElementMenu getModelMenu() {
		return modelMenu;
	}
	public void setModelMenu(ElementMenu modelMenu) {
		this.modelMenu = modelMenu;
	}

}
