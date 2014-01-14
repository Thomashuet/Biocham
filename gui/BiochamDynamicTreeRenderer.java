package fr.inria.contraintes.biocham;

import fr.inria.contraintes.biocham.commandLine.SimpleCommandLine;
import fr.inria.contraintes.biocham.documentation.TutorialArticle;
import fr.inria.contraintes.biocham.modelData.AbstractionView;
import fr.inria.contraintes.biocham.modelData.SimulationView;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;




/**
 * A renderer class for the main explorer tree of the gui.
 * 
 * */
public class BiochamDynamicTreeRenderer extends DefaultTreeCellRenderer {
    
    public BiochamDynamicTreeRenderer() {
    	setTextNonSelectionColor(Utils.foregroundColor);
        setFont(Utils.treeExplorerFont);            
    }
    
   
    public BiochamDynamicTreeRenderer(String domain){
    	        	
    	if(domain.equals("rules")){
        	setBackgroundSelectionColor(Utils.backgroundColor);
        	setBackgroundNonSelectionColor(Utils.backgroundColor);
        	setBorderSelectionColor(Utils.backgroundColor);
        	setTextSelectionColor(Color.black);
        	setTextNonSelectionColor(Color.black);
        	setToolTipText("Show expanded rules...");
    	}
    }

    public Component getTreeCellRendererComponent(
                        JTree tree,
                        Object value,
                        boolean selected,
                        boolean expanded,
                        boolean leaf,
                        int row,
                        boolean hasFocus) {

        super.getTreeCellRendererComponent(
                        tree, value, selected,
                        expanded, leaf, row,
                        hasFocus);
                	
        DefaultMutableTreeNode node =(DefaultMutableTreeNode)value;
        
        if(tree.getName().contains("Biocham Models")){
        	if (isModelName(value)==1 ) { // it WILL set the icons depending if its a root,branch or leaf node.
                setIcon(Icons.icons.get("SSicon.png"));       	 	
            } else if(isModelName(value)==2){
            	setIcon(Icons.icons.get("dark-blue-circle.png"));          	
            }else if(isModelName(value)==14){
            	setIcon(Icons.icons.get("molecule1.png"+0.2));
            }else if(isModelName(value)==4){
            	setIcon(Icons.icons.get("taskic.jpg"+0.5));
            	setToolTipText("Run simulations, work with traces, compare simulation plots....");
            }else if(isModelName(value)==3){
            	
            	BiochamModelElement me=(BiochamModelElement)node.getUserObject();            	
            	if(me!=null && me.getElementName()!=null){
            		if(me.getElementName().equals("Events")){                	
                		setIcon(Icons.icons.get("gEditorIcon.png"+0.4));                		
                		setToolTipText("Lists all the declared events.");
                	}else if(me.getElementName().contains("Reaction")){
                		setIcon(Icons.icons.get("taskic.jpg"+0.5));
                    	setToolTipText("Lists the current set of reactions");
                    }
                    else if(me.getElementName().equals("Declarations")){
                    	setIcon(Icons.icons.get("gEditorIcon.png"+0.4));
                    	setToolTipText("Lists all the declarations of phosphorylated forms of molecules ");		
                    }
                    else if(me.getElementName().equals("Macros")){
                    	setIcon(Icons.icons.get("gEditorIcon.png"+0.4));
                    	setToolTipText("Shows the values of all known macros ");		
                    }
                    else if(me.getElementName().equals("Molecules")){
                    	setIcon(Icons.icons.get("gEditorIcon.png"+0.4));
                    	setToolTipText("Lists the molecules contained in all instances of the current set of rules");		
                    }
                    else if(me.getElementName().equals("Parameters")){
                    	setIcon(Icons.icons.get("gEditorIcon.png"+0.4));
                    	setToolTipText("Shows the values of all known parameters ");		
                    }               
                    else if(me.getElementName().contains("Initial")){
                    	setIcon(Icons.icons.get("gEditorIcon.png"+0.4));
                    	setToolTipText("Lists the objects which are present (including their initial concentration) and absent from the initial state");		
                    }
                    else if(me.getElementName().equals("Boolean Temporal Properties")){
                    	setIcon(Icons.icons.get("taskic.jpg"+0.5));
                    	setToolTipText("Lists the current set of CTL formulae of the model ");		
                    } else if(me.getElementName().equals("Numerical Temporal Properties")){
                    	setIcon(Icons.icons.get("taskic.jpg"+0.5));
                    	setToolTipText("Lists the current set of LTL formulae of the model ");
                    }
                    else if(me.getElementName().equals("Conservation Laws")){
                    	setIcon(Icons.icons.get("gEditorIcon.png"+0.4));
                    	setToolTipText("Lists all the mass conservation laws for all the molecules in the model ");		
                    }else if(me.getElementName().contains("Reaction Rule Model")){
                    	setIcon(Icons.icons.get("taskic.jpg"+0.5));
                    }
                	me=null;
            	}
           	 	
            }else if(isModelName(value)==8){//WARNINGS            	
            	setToolTipText("Lists the warnings and errors occured in the model.");		            	
            	setIcon(Icons.icons.get("page_white_error.png"+1.5));
            }else if(isModelName(value)==9){//COMMAND_LINE
            	setToolTipText("Command-line communication with Biocham");	
            	setIcon(Icons.icons.get("application_xp_terminal.png"+1.4));
            }else if(isModelName(value)==0){
            	setIcon(null);
            }
        }else if(tree.getName().contains("Documentation")){
        	if(isModelName(value)==10){
            	setToolTipText("Biocham articles and tutorials");		            	
            	setIcon(Icons.icons.get("Books-icon.gif"));
            }else if(isModelName(value)==11){
            	setIcon(Icons.icons.get("21.png"));
            	DefaultMutableTreeNode n =(DefaultMutableTreeNode)value;
                String name=n.getUserObject().toString();
            	setToolTipText(name);
            	n=null;
            	name=null;
            }
        }else{
        	if(isModelName(value)==15){            	
            	setIcon(null);
            }else if(isModelName(value)==0){
            	setIcon(null);
            }
        }
        
        
        
        return this;
    }
    
    protected int isModelName(Object value) {
                          
        if(((DefaultMutableTreeNode)value).getUserObject() instanceof BiochamModel){
        	return 1;
        }else if(((DefaultMutableTreeNode)value).getUserObject() instanceof BiochamModelElement){
        	return 3;
        }else if(((DefaultMutableTreeNode)value).getUserObject() instanceof BiochamWarnings){
        	return 8;
        }else if(((DefaultMutableTreeNode)value).getUserObject() instanceof SimpleCommandLine){
        	return 9;
        }else if(value.toString().equals("Documentation")){
        	return 10;
        }else if(value.toString().equals("Biocham Models")){
        	return 2;
        }else if(((DefaultMutableTreeNode)value).getUserObject() instanceof TutorialArticle){
        	return 11;
        }else if(((DefaultMutableTreeNode)value).getUserObject() instanceof BiochamDynamicTreeSeparator){
        	return 14;
        }else if(((DefaultMutableTreeNode)value).getUserObject() instanceof AbstractionView){
        	return 4;        		
        }else if(((DefaultMutableTreeNode)value).getUserObject() instanceof SimulationView){
        	return 4;
        }else if(((DefaultMutableTreeNode)value).getUserObject() instanceof String){
        	return 15;
        }
        else return 0;
       
    
    }
}
