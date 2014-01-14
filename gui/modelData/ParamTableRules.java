package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.WorkbenchArea;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.DeleteButton;
import fr.inria.contraintes.biocham.customComponents.ModifyButton;
import fr.inria.contraintes.biocham.dialogs.DialogAddSpecification;
import fr.inria.contraintes.biocham.graphicalEditor.Parser_SBGNRule2BiochamRule;
import fr.inria.contraintes.biocham.menus.BiochamMenuBar;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Vector;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTree;
import javax.swing.Spring;
import javax.swing.SpringLayout;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;





public class ParamTableRules implements Parameters, ActionListener{
	
	private final static int H_CONST=70;
	private final static int W_CONST=10;
	Vector<Rule> rules;
	WorkbenchArea biocham;
	private JPanel panel;
	BiochamModel model;
	BiochamModelElement element;
	int savedResponses;
	boolean deleting=false, adding=false;
	ArrayList<String> uknownParams;
	Spring maxSpring;
	int north=0;
	HashMap<Integer,String> parentRules;
	boolean newAdded=false;
	boolean ignoreUndefinedParametersWarnings=false;
	boolean addRule=false;
	boolean fromGraph=false;
	int rulesCounter=0;	
	boolean dontDraw=false;
	int counter=0;
	ModifyRule modifyListener;
	DeleteRule deleteListener;
	private boolean forDeleting=false;
	RulesModel rulesModel;
	RulesView view;
	
	
	public RulesView getView() {
		return view;
	}
	public void setView(RulesView view) {
		this.view = view;
	}
	public RulesModel getRulesModel() {
		return rulesModel;
	}
	public void setRulesModel(RulesModel rmodel) {
		this.rulesModel = rmodel;
	}
	private int calculatePreferredPanelHeight(){
		int h=0;
		h=rules.size()*ParamTableRules.H_CONST;
		Utils.debugMsg("h="+h);
		return h;
	}
	private int calculatePreferredPanelWidth(){
		int w=0;
	
		for(int i=0;i<rules.size();i++){
			if(w<rules.get(i).getName().length()){
				w=rules.get(i).getName().length();
			}
		}
		w*=ParamTableRules.W_CONST;
		Utils.debugMsg("w="+w);
		return w;
	}
	
	public void setPanelPreferredSize(){
		Dimension d=null;
		
		if(rules.size()>1){
			d=new Dimension(calculatePreferredPanelWidth(),calculatePreferredPanelHeight());
		}else{
			d=new Dimension(100,100);
		}
		
		if(panel.getParent()!=null){
			panel.getParent().setPreferredSize(d);
			Utils.debugMsg("PARENT IS NOT NULL");
		}else{
			Utils.debugMsg("PARENT ISSSSSS NULL");
		}/*panel.setPreferredSize(null);
		panel.revalidate();
		panel.repaint();*/
		/*if(getPanel().getParent()!=null) getPanel().getParent().validate();
		//getPanelCurrentX();getPanelCurrentY();
		SwingWorker sw=new SwingWorker(){

			@Override
			public Object construct() {
				Dimension d=new Dimension(getPanelCurrentX(),getPanelCurrentY());
				getPanel().setPreferredSize(d);
				//getPanel().setLayout (null);
				
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				d=null;
				return null;
			}

			@Override
			public void finished() {
				//getPanel().getParent().validate();
				
			}};
		sw.start();
		
		
		//if(rules.size()<4) getPanel().setMaximumSize(new Dimension(100,80));
		//getPanel().setMaximumSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));
		//getPanel().revalidate();
		//getPanel().repaint();
*/		
		
	}
	
	
	/*public int getPanelCurrentX(){
		int x=(int) getPanel().getPreferredSize().getWidth();
		
		int x=0;
		Spring maxSpring =null;// Spring.constant(10);
		SpringLayout layout = (SpringLayout) getPanel().getLayout();
		int i=1;
		if(getPanel().getComponentCount()>3){
			i=3;
		}
		maxSpring=layout.getConstraint(SpringLayout.WEST, getPanel().getComponent(getPanel().getComponentCount()-i));
		x=maxSpring.getValue()+80;
		
		System.out.println("\nx="+x+"\n");
		return x;
	}
	public int getPanelCurrentY(){
		int y=(int) getPanel().getPreferredSize().getWidth();
		
		int y=0;
		Spring maxSpring =null;// Spring.constant(10);
		SpringLayout layout = (SpringLayout) getPanel().getLayout();
		maxSpring=layout.getConstraint(SpringLayout.NORTH, getPanel().getComponent(getPanel().getComponentCount()-1));
		y=maxSpring.getValue()+80;
		
		
		System.out.println("\ny="+y+"\n");
		return y;
	}*/
	
	public ParamTableRules(BiochamModel m,WorkbenchArea workbench, JPanel p){
		
		biocham=workbench;
		model=m;
		setPanel(p);
		rules=new Vector<Rule>();
		element=model.getRules();
		savedResponses=-1;
		uknownParams=new ArrayList<String>();
		parentRules=new HashMap<Integer,String>();
		Utils.modern.setBorderThickness(3);
		Utils.modern.enableAntiAliasing(true);	
		modifyListener=new ModifyRule();
		deleteListener=new DeleteRule();
		
		rulesModel=new RulesModel(model);
		view=new RulesView(BiochamMainFrame.frame,rulesModel);
	}
	
	

	public String getName(int i) {
		return rules.get(i).getName();
	}
	
	public String getValue(int i) {
		return rules.get(i).getValue();
	}
	
	
	public int indexOf(String paramName) {
		int i=0;
	    while (i<rules.size() && !getName(i).equals(paramName))
	       i++;
	    if (i == rules.size())
	       return -1;
	    return i;
	}
	
	public int indexOfExtendedPart(String paramName) {
		int i=0;
	    while (i<rules.size() && !getValue(i).equals(paramName))
	       i++;
	    if (i == rules.size())
	       return -1;
	    return i;
	}
	
	public void resetParameter(String s) {
		//as the rule can't be modified, it can't be reseted to its original value too.
	}
	
	
	public void setValue(ArrayList list) {
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		String arg1=((String) list.get(0)).trim();
		String arg2=((String) list.get(1)).trim();
		
		int i = indexOf(arg1);
		int j = indexOfExtendedPart(arg2);
		
		rulesModel.addRule(arg1, arg2);
		
	    if(i<0 || j<0){
	    
	    	Rule rule=new Rule(arg1,arg2);
	    	rules.add(rule);
	    	
	    	
			BiochamModelElement element=model.getModelElement("Reactions");
			
			if(i<0){ 
			
				BiochamDynamicTree tree=new BiochamDynamicTree(arg1.trim());
				tree.addRuleObject(rule);			
				tree.setName(arg1);
				tree.tree.setName(arg1.trim());
				element.addDtree(tree);
				getPanel().add(tree.tree);				
				parentRules.put(rulesCounter,arg1.trim());
				rulesCounter++;
								
			}else if(i>=0 && j<0){		
				Component[] comps=getPanel().getComponents();
				for(int k=0;k<comps.length;k++){
					if(comps[k].getName().equals(arg1)){	
						BiochamDynamicTree dtree=element.getDtree(arg1);
						dtree.addRuleObject(rule);									
						//getPanel().setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
						//getPanel().revalidate();	
						break;
					}
				}
				comps=null;
			}
			//getPanel().setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
			//getPanel().revalidate();	
	    }else{
	    	//JOptionPane.showMessageDialog(BiochamMainFrame.frame,"\nThe rule "+arg1+" is already definied.\n");
	    }
	    setFromGraph(false);
	    refreshAfterAddingNew();
		setNewAdded(false);
		
		//setPanelPreferredSize();	
		//getPanel().revalidate();	
		
	}
	
	
	public int size() {
		return rules.size();
	}
	
	public void setModified(Component comp) {
		// There is no point yet of rule to be modified.
	}
	
	
	
public class Rule{
		
		private String name,value;
		
		Rule(String n,String v){
			name=n;  //the original rule
			value=v; //the expanded rule
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public String getValue() {
			return value;
		}

		public void setValue(String value) {
			this.value = value;
		}
		
		public String toString() {		  
		      return getValue();
		}
		
}
class ModifyRule extends MouseAdapter{
	public void mouseClicked(MouseEvent e) {
		Component button=(Component) e.getSource();
		String name=button.getName();
		modifyRule(name,true,true);
		button=null;
		name=null;
	}
}
class DeleteRule extends MouseAdapter{
	public void mouseClicked(MouseEvent e) {
		int answ=JOptionPane.showConfirmDialog(BiochamMainFrame.frame,"Are you sure you want to delete this reaction? ","Delete reaction rule",JOptionPane.YES_NO_OPTION);
		if(answ==JOptionPane.YES_OPTION){
			Component button=(Component) e.getSource();
			String name=button.getName();
			deleteRule(name,true,true,true);    
			button=null;
			name=null;
		}
		
	}
}


synchronized public JPanel refreshPanel(ArrayList cs) {

	
	int northBefore=25;
	Spring maxSpring = Spring.constant(BiochamModel.MIDDLE);
	int size=cs.size();
	SpringLayout layout = (SpringLayout) getPanel().getLayout();
	if(size!=0){
		
		JTree jt;	
		ArrayList<JTree> trees=new ArrayList<JTree>();		
		for(int i=0;i<size;i++){
			if(cs.get(i) instanceof JTree){
				trees.add((JTree)cs.get(i));
			}
		}		
		size=trees.size();		
		JTree treeBefore=null;
		for(int t=0;t<size;t++){
			
				jt=trees.get(t);	
				jt.setToolTipText("Show expanded rules...");
				String s=jt.getName();
				ArrayList<TreeNode> nodes = new ArrayList<TreeNode>();
				TreeNode root = (TreeNode) jt.getModel().getRoot();
				nodes.add(root);
				appendAllChildrenToList(nodes, root, true);
				jt.collapseRow(0);
				
				panel.add(jt);
				
				ModifyButton but1=new ModifyButton();				
		    	but1.setName(s);
		    	but1.addMouseListener(modifyListener);
		    	
		    	panel.add(but1);
		    
		    	DeleteButton b2=new DeleteButton();
		    	b2.setName(s);		    
		    	b2.addMouseListener(deleteListener);    	
				
		    	panel.add(b2);
				
				int nrthBefore=0;
				if(treeBefore!=null){
					nrthBefore=layout.getConstraint(SpringLayout.NORTH,treeBefore).getValue();
				}				
				int p=1;
				if(t==0){
					northBefore=25;
				}else{					
					p+=((DefaultMutableTreeNode)treeBefore.getModel().getRoot()).getChildCount();
					northBefore=nrthBefore+p*treeBefore.getHeight()+20;
				}			
				
				layout.putConstraint(SpringLayout.WEST, but1, 5, SpringLayout.WEST, panel);				
				layout.putConstraint(SpringLayout.NORTH, but1, northBefore,SpringLayout.NORTH, panel);
				layout.putConstraint(SpringLayout.WEST, b2, 5, SpringLayout.EAST, but1);				
				layout.putConstraint(SpringLayout.NORTH, b2, northBefore,SpringLayout.NORTH, panel);
				layout.putConstraint(SpringLayout.WEST, jt, 10, SpringLayout.EAST, b2);
				layout.putConstraint(SpringLayout.NORTH, jt, northBefore,SpringLayout.NORTH, panel);
				
				
				treeBefore=jt;			
				nodes.clear();
				nodes=null;
				s=null;
				root=null;
		}
		
		/*Component[] comps=getPanel().getComponents();
		
		for(int i=0;i<comps.length/3;i++){		
			Spring x=Spring.sum(Spring.sum(layout.getConstraints(panel.getComponent(3*i)).getWidth(),Spring.constant(30)),Spring.constant(20));
			System.out.println("********X-Spring="+x.getValue());
			maxSpring = Spring.max(maxSpring, x);
			System.out.println("********maxX-Spring="+maxSpring.getValue());
			// int v1=maxSpring.getValue();
		}
		for(int i=0;i<comps.length/3;i++){			
			layout.putConstraint(SpringLayout.WEST, panel.getComponent(3*i+1), maxSpring, SpringLayout.WEST, panel);			
		}
		int v1=maxSpring.getValue();
		System.out.println("********maxX-Spring="+v1);
		comps=null;
		*/
		
		trees.clear();
		trees=null;
		
	
	}
	
	
	String toolTipText="<html><i>Add a reaction rule to the current set of rules if any.</i></html>";
    CustomToolTipButton addButton=new CustomToolTipButton("Add",toolTipText);	
    addButton.setBalloonToolTipVisible(false);
	addButton.setName("Add");
	addButton.setActionCommand("addRule");
	addButton.addActionListener(this);
	
	toolTipText="<html><i>Shows kinetics in a separate window.</i></html>";
    CustomToolTipButton showKineticsButton=new CustomToolTipButton("Show Kinetics",toolTipText);	
    showKineticsButton.setBalloonToolTipVisible(false);
    showKineticsButton.setName("showKinetics");
    showKineticsButton.setActionCommand("showKinetics");
    showKineticsButton.addActionListener(biocham.tree.treeListener);
    
    toolTipText="<html><i>Export the kinetics to a file.</i></html>";
    CustomToolTipButton exportKineticsButton=new CustomToolTipButton("Export Kinetics",toolTipText);	
    exportKineticsButton.setBalloonToolTipVisible(false);
    exportKineticsButton.setName("exportKinetics");
    exportKineticsButton.setActionCommand("exportKinetics");
    exportKineticsButton.addActionListener(biocham.tree.treeListener);
    
    toolTipText="<html><i>Deletes all the reactions' rules.</i></html>";
    CustomToolTipButton deleteAllButton=new CustomToolTipButton("Delete All",toolTipText);	
    deleteAllButton.setBalloonToolTipVisible(false);
    deleteAllButton.setName("deleteAll");
    deleteAllButton.setActionCommand("deleteAll");
    deleteAllButton.addActionListener(this);
    
    toolTipText="<html><i>Launches the graphical reaction editor in a separate window.</i></html>";
    CustomToolTipButton rgeButton=new CustomToolTipButton("Graphical Editor",toolTipText);	
    rgeButton.setBalloonToolTipVisible(false);
    rgeButton.setName("launchGraphicalEditor");
    rgeButton.setActionCommand("launchGraphicalEditor");
    rgeButton.addActionListener(biocham.tree.treeListener);
	
    /*JLabel refreshButton=new JLabel();
    refreshButton.setIcon(Icons.icons.get("Refresh3.png"));
    refreshButton.setName("refresh");	    
    refreshButton.setText("Screen Refresh");
    refreshButton.setForeground(Utils.refreshedColor);
    refreshButton.setToolTipText("Click to Refresh the Screen");
    refreshButton.addMouseListener(new MouseAdapter(){
    	public void mouseClicked(MouseEvent me) { 
            refreshAfterAddingNew();			 
        }
    });	*/
	if(panel.getComponents().length>0 && !(panel.getComponents()[0] instanceof JButton)){
		
		 //int len=getPanel().getComponents().length;
		 	
		 //Spring n=layout.getConstraint(SpringLayout.NORTH,getPanel().getComponent(len-1));
		 
		 panel.add(addButton);
		 panel.add(showKineticsButton);
		 panel.add(exportKineticsButton);
		 panel.add(deleteAllButton);
		 panel.add(rgeButton);
		 int north=(northBefore+60);
		 layout.putConstraint(SpringLayout.NORTH, addButton,north,SpringLayout.NORTH,panel);
         layout.putConstraint(SpringLayout.WEST, addButton, LEFT_OFF, SpringLayout.WEST, panel);
         layout.putConstraint(SpringLayout.NORTH, showKineticsButton, 0,SpringLayout.NORTH, addButton);
         layout.putConstraint(SpringLayout.WEST, showKineticsButton, 10, SpringLayout.EAST, addButton);
         layout.putConstraint(SpringLayout.NORTH, exportKineticsButton, 0,SpringLayout.NORTH, showKineticsButton);
         layout.putConstraint(SpringLayout.WEST, exportKineticsButton, 10, SpringLayout.EAST, showKineticsButton);
         layout.putConstraint(SpringLayout.NORTH, deleteAllButton, 0,SpringLayout.NORTH, exportKineticsButton);
         layout.putConstraint(SpringLayout.WEST, deleteAllButton, 10, SpringLayout.EAST, exportKineticsButton);
         layout.putConstraint(SpringLayout.NORTH, rgeButton, 0,SpringLayout.NORTH, deleteAllButton);
         layout.putConstraint(SpringLayout.WEST, rgeButton, 10, SpringLayout.EAST, deleteAllButton);
        
         
	 }else{
		 panel.add(addButton);
		 panel.add(rgeButton);
		 
		 layout.putConstraint(SpringLayout.NORTH, addButton, BiochamModel.HEIGHT,SpringLayout.NORTH, panel);
		 layout.putConstraint(SpringLayout.NORTH, rgeButton, BiochamModel.HEIGHT,SpringLayout.NORTH, panel);
		 layout.putConstraint(SpringLayout.WEST, addButton, 10, SpringLayout.WEST, panel);
		 layout.putConstraint(SpringLayout.WEST, rgeButton, 10, SpringLayout.EAST, addButton);
		 
	 }
	//model.getRules().setSumChildren(sumChildren);	
	//getPanel().setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
	/*if(!forDeleting){
		setPanelPreferredSize();
	}else{
		forDeleting=false;
		getPanel().revalidate();
		getPanel().repaint();
	}*/
	getPanel().revalidate();
	getPanel().repaint();
	//getPanel().revalidate();		
	BiochamMenuBar.refreshMenusContext(model);
	
	return panel;

}

	
	
	
	
private static void appendAllChildrenToList(ArrayList<TreeNode> nodes, TreeNode parent, boolean getChildChildren) {
	Enumeration children = parent.children();
	if (children != null) {
		while (children.hasMoreElements()) {
			TreeNode node = (TreeNode) children.nextElement();
			nodes.add(node);			
			if (getChildChildren) {
				appendAllChildrenToList(nodes, node, getChildChildren);
			}
		}
	}
}





	public void changeRules(final String oldRule, final String newRule){
	
		SwingWorker sw=new SwingWorker(){

			String s="";
			@Override
			public Object construct() {
				setFromGraph(true);
				boolean has=false;
				String ruleToDelete=oldRule;
				
				if(oldRule.startsWith("{") && oldRule.endsWith("}")){
					ruleToDelete=oldRule.substring(1,oldRule.length()-1);				
				}
				
					
				if(oldRule.contains("<=")){
					String tmp0="";
					int ind=0;
					if(ruleToDelete.contains("for")){
						tmp0=ruleToDelete.substring(0,ruleToDelete.indexOf("for")+2);
						ind=ruleToDelete.indexOf("for")+2;
					}
					String r=ruleToDelete.substring(ind,ruleToDelete.indexOf("<"));
					String tmp0_1="";
					if(ruleToDelete.contains("=[")){
						tmp0_1=ruleToDelete.substring(ruleToDelete.indexOf("<="),ruleToDelete.indexOf("]"));
					}
					String p=ruleToDelete.substring(ruleToDelete.lastIndexOf(">"));
					ruleToDelete=tmp0+r+"="+tmp0_1+"=>"+p+","+tmp0+p+"="+tmp0_1+"=>"+r;
				}
				
				s="delete_rules({"+ruleToDelete+"}).\n";				
				model.sendToBiocham(s,"rules");	
				
				Component[] comps=getPanel().getComponents();
				ArrayList cs=new ArrayList();   
				for(int i=0;i<comps.length;i++){
				  	if(comps[i].getName().equals(oldRule)){
				   		cs.add(comps[i]);
				   	}
				}
				for(int i=0;i<cs.size();i++){
				   	if(cs.get(i) instanceof JTree){
				   		JTree deletedTree=(JTree)cs.get(i);	    			    		 			    					    			    		
						BiochamModelElement element=model.getModelElement("Reactions");
						int ind=element.getNodeIndex(oldRule);
						if(ind>=0){
							element.removeNodeDtree(ind);
						}
						JLabel delbut=(JLabel)cs.get(i+1);
				    	getPanel().remove(deletedTree);
				    	getPanel().remove(delbut);	
				    	getPanel().validate();	
				   	}
				}
				forDeleting=true;
				int siz=rules.size();
				ArrayList<Integer> al=new ArrayList<Integer>();
				String cmd="";
				for(int i=0;i<siz;i++){    					
					if(rules.get(i).getName().equals(oldRule)){
						if(has){
							cmd+=",";
						}
						cmd+=rules.get(i).getValue();
						has=true;
						al.add(i);
					}
				}  
				siz=al.size();
				for(int i=siz-1;i>=0;i--){
					rules.removeElementAt(al.get(i));
				}
				al.clear();
				al=null;
				//oldRule+=","+cmd;			
				comps=getPanel().getComponents();
				cs.clear();
				for(int i=0;i<comps.length;i++){	
					if((comps[i] instanceof JTree)){
						cs.add(comps[i]);
					}				
				}
				comps=null;
				getPanel().removeAll();
				refreshPanel(cs);
				cs.clear();
				cs=null;		
				clearUknownParams();
				/*try{
					getPanel().getParent().validate();
					getPanel().getParent().repaint();
				}catch(Exception e){
					e.printStackTrace();
				}*/
				return null;
			}

			@Override
			public void finished() {
//				at the end..
				s="add_rules("+newRule+").\n"+"list_molecules.\n";		
				Parser_SBGNRule2BiochamRule.setDontDraw(true);	
			    model.sendToBiocham(s,"rules");
			    
				
			}};
	
		sw.start();

		
	}
	
	public void modifyRule(final String nmm,final boolean inGraphAlso, final boolean fromEverywhere){
		
		String s = (String)JOptionPane.showInputDialog(
		                    BiochamMainFrame.frame,
		                    "",
		                    "Rule modification",
		                    JOptionPane.PLAIN_MESSAGE,
		                    null,
		                    null,
		                    nmm);

//		If a string was returned, say so.
		if ((s != null) && (s.length() > 0)) {
		    deleteRule(nmm,true,true,false);
		    sendRuleToBiocham(s);	   
		}

	}
	
	public void deleteRule(final String nmm,final boolean inGraphAlso, final boolean fromEverywhere,boolean verifyReversible) {
		
		boolean exit=false;
		forDeleting=true;
		
		try{
			
			
			String s1=null, s2=null;
			String ruleToDelete=nmm;
			if(nmm.startsWith("{") && nmm.endsWith("}")){
				ruleToDelete=nmm.substring(1,nmm.length()-1);				
			}
			
			if(nmm.contains("<=")){
				String rule1=null,rule2=null;
				for(int i=0;i<rules.size();i++){
					if(rules.get(i).getName().equals(ruleToDelete)){
						if(rule1==null){
							rule1=rules.get(i).getValue();	
						}else{
							rule2=rules.get(i).getValue();
							break;
						}
						
					}
				}
				JCheckBox choice1 = new JCheckBox(rule1);
				choice1.setName(rule1);				
				JCheckBox choice2 = new JCheckBox(rule2);				
				choice2.setName(rule2);				
				
				if(verifyReversible){			 
				
					int answer=JOptionPane.showConfirmDialog(BiochamMainFrame.frame,new Object[]{"Select which direction you want to delete:",choice1,choice2},"Delete Rule",JOptionPane.OK_CANCEL_OPTION);
					if(answer==JOptionPane.OK_OPTION){
					
					//delete all the rule....
			    	
						String tmp0="";
						int ind=0;
						if(ruleToDelete.contains("for")){
							tmp0=ruleToDelete.substring(0,ruleToDelete.indexOf("for")+4);
							ind=ruleToDelete.indexOf("for")+4;
						}
						String r=ruleToDelete.substring(ind,ruleToDelete.indexOf("<"));
						String tmp0_1="";
						if(ruleToDelete.contains("=[")){
							tmp0_1=ruleToDelete.substring(ruleToDelete.indexOf("["),ruleToDelete.indexOf("]")+1);
						}
						String p=ruleToDelete.substring(ruleToDelete.lastIndexOf(">")+1);
						if(tmp0_1.equals("")){
							ruleToDelete=tmp0+r+"=>"+p+","+tmp0+p+"=>"+r;
						}else{
							ruleToDelete=tmp0+r+"="+tmp0_1+"=>"+p+","+tmp0+p+"="+tmp0_1+"=>"+r;
						}
					    if(choice1.isSelected() && !choice2.isSelected()){
					    	//delete all add just one.....				    
					    	s2="add_rules("+choice2.getName()+").\n list_molecules.\n";		
					    }else if(!choice1.isSelected() && choice2.isSelected()){
					    	//delete all add just one....				    	
					    	s2="add_rules("+choice1.getName()+").\n list_molecules.\n";
					    }else if(!choice1.isSelected() && !choice2.isSelected()){
					    	//do nothing.....
					    	JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You didn't choose any rule's direction to delete.");
					    	exit=true;
					    }
					
					}else{
						exit=true;
					}
					}else{
					//delete all the rule....
			    	
						String tmp0="";
						int ind=0;
						if(ruleToDelete.contains("for")){
							tmp0=ruleToDelete.substring(0,ruleToDelete.indexOf("for")+4);
							ind=ruleToDelete.indexOf("for")+4;
						}
						String r=ruleToDelete.substring(ind,ruleToDelete.indexOf("<"));
						String tmp0_1="";
						if(ruleToDelete.contains("=[")){
							tmp0_1=ruleToDelete.substring(ruleToDelete.indexOf("["),ruleToDelete.indexOf("]")+1);
						}
						String p=ruleToDelete.substring(ruleToDelete.lastIndexOf(">")+1);
						if(tmp0_1.equals("")){
							ruleToDelete=tmp0+r+"=>"+p+","+tmp0+p+"=>"+r;
						}else{
							ruleToDelete=tmp0+r+"="+tmp0_1+"=>"+p+","+tmp0+p+"="+tmp0_1+"=>"+r;
						}
					}
			}
			
			if(!exit){
				s1="delete_rules({"+ruleToDelete+"}).\n";	
				model.sendToBiocham(s1,"rules");
				
				Component[] comps=getPanel().getComponents();
				ArrayList cs=new ArrayList();   			    
				for(int i=0;i<comps.length;i++){
				  	if(comps[i].getName().equals(nmm)){
				   		cs.add(comps[i]);
				   	}
				}		
				for(int i=0;i<cs.size();i++){
				   	if(cs.get(i) instanceof JTree){
				   		JTree deletedTree=(JTree)cs.get(i);
				   	    			    		 			    					    			    		
						BiochamModelElement element=model.getModelElement("Reactions");
						int ind=element.getNodeIndex(nmm);
						if(ind>=0){
							element.removeNodeDtree(ind);
						}
						JLabel delbut=(JLabel)cs.get(i+1);
						JLabel delbut2=(JLabel)cs.get(i+2);
				    	getPanel().remove(deletedTree);
				    	getPanel().remove(delbut);
				    	getPanel().remove(delbut2);
				    		    			    		
				   	}
				}
				try{
					rules.removeElementAt(indexOf(nmm));
				}catch(Exception e){
					e.getStackTrace();
				}
			
				if(nmm.contains("<=")){
					try{
						rules.removeElementAt(indexOf(nmm));
					}catch(Exception e){}
				}
							
				comps=getPanel().getComponents();
				cs.clear();
				for(int i=0;i<comps.length;i++){	
					if((comps[i] instanceof JTree)){
						cs.add(comps[i]);
					}					
				}
				comps=null;
				getPanel().removeAll();
				refreshPanel(cs);
				cs.clear();
				cs=null;
			
				clearUknownParams();
				
				for(int i=0;i<parentRules.size();i++){
					if(parentRules.get(i)!=null && parentRules.get(i).equals(nmm)){
						parentRules.remove(i);
						break;
					}
				}
				if(nmm.contains("<=")){
					for(int i=0;i<parentRules.size();i++){
						if(parentRules.get(i)!=null && parentRules.get(i).equals(nmm)){
							parentRules.remove(i);
							break;
						}
					}
				}
				if(inGraphAlso && ruleToDelete!=null){
					model.getGraphEditor().getGraph().deleteReaction(nmm,fromEverywhere);
					
				}	
				
				
				if(s2!=null){
					model.sendToBiocham(s2,"rules");
				}
			}
			
			
							
		} catch(Exception e){
			if(!exit){
				model.getGraphEditor().getGraph().deleteReaction(nmm,fromEverywhere);
			}			
			e.printStackTrace();
		}	
		
		BiochamMenuBar.refreshMenusContext(model);
		refreshAfterAddingNew();
	}
		

public void refreshAfterAddingNew() {
	
	
	ArrayList<Component> cs=new ArrayList<Component>();
	Component[] comps=getPanel().getComponents();
	for(int i=0;i<comps.length;i++){
		if((comps[i] instanceof JTree)){
			cs.add(comps[i]);
			
		}
	}
	getPanel().removeAll();			
	setPanel(refreshPanel(cs));			
	
	int o=getUknownParams().size();	    	
	if(o>0){
		String s="Attention! These parameters have to be declared:"+"\n<html><font color=#4EE2EC>";
		boolean exists=false;
		for(int i=0;i<o;i++){
			s+="      "+getUknownParams().get(i);
			//add the rows for the uknown parameters.....
			int ukn=((ParamTableParameters)model.getParameters().getParamTable()).getUknown().size();
			exists=false;
			for(int j=0;j<ukn;j++){						
				if(((ParamTableParameters)model.getParameters().getParamTable()).getUknown().get(j).equals(getUknownParams().get(i))){
					exists=true;
				}    					
			}
			if(!exists){
				ArrayList<String> sl=new ArrayList<String>();
				sl.add(getUknownParams().get(i));
				sl.add("You have to define the value of this parameter...");
				((ParamTableParameters)model.getParameters().getParamTable()).addUknownParameter(sl);
				sl.clear();
				sl=null;
			}
			if(i<o-1){
				s+=", ";
			}
			int m=(i+1)%4;
			if(m==0){
				s+="<br>      ";
			}
		}
		s+="</font><br><br></html>";
		if(!isIgnoreUndefinedParametersWarnings() && !isAddRule()){
			JOptionPane.showMessageDialog(BiochamMainFrame.frame, s);
			setAddRule(false);
		}
		//check if all molecules are declared....
		if(!exists){
			int molsSize=((ParamTableMolecules)model.getMolecules().getParamTable()).molecules.size();
			int initConcSize=((ParamTableInitConc)model.getInitConditions().getParamTable()).getInitConcentrations().size();
			s="";
			for(int i=0;i<molsSize;i++){
				if(initConcSize==0){
					s+="absent("+((ParamTableMolecules)model.getMolecules().getParamTable()).molecules.get(i).getMoleculeName()+").\n";
				}else{
				for(int j=0;j<initConcSize;j++){
					if(!((ParamTableInitConc)model.getInitConditions().getParamTable()).getInitConcentrations().get(j).getName().equals(((ParamTableMolecules)model.getMolecules().getParamTable()).molecules.get(i).getMoleculeName())){
						s+="absent("+((ParamTableMolecules)model.getMolecules().getParamTable()).molecules.get(i).getMoleculeName()+").\n";
					}
				}
				}
			}				
			model.sendToBiocham(s,"rules");	
		}
	}

	setPanelPreferredSize();	
	//getPanel().revalidate();		
	BiochamMenuBar.refreshMenusContext(model);
}

public int getSavedResponses() {
	return savedResponses;
}

public void setSavedResponses(int savedResponses) {
	this.savedResponses = savedResponses;
}

public void resetSavedResponses() {
	savedResponses=-1;
}

public boolean isDeleting() {
	return deleting;
}

public void setDeleting(boolean deleting) {
	this.deleting = deleting;
}

public boolean isAdding() {
	return adding;
}

public void setAdding(boolean adding) {
	this.adding = adding;
}

public Vector<Rule> getRules() {
	return rules;
}

public void addRule(Rule rule) {
	rules.add(rule);
	
}

public Rule createNewRule(String p, String v) {
	return new Rule(p,v);
}

public void disposeElements() {
	
	rules.clear();
	rules=null;
	biocham=null;
	setPanel(null);
	model=null;
	element=null;
	uknownParams.clear();
	uknownParams=null;
	parentRules.clear();
	parentRules=null;
}

public ArrayList<String> getUknownParams() {
	return uknownParams;
}
public void clearUknownParams() {
	uknownParams.clear();
}

public void actionPerformed(ActionEvent e) {
	if(e.getActionCommand()=="addRule"){
		
		setFromGraph(false);
		model.getGraphEditor().getGraph().setAllAdded(false);
		addParameter();		
		BiochamMenuBar.refreshMenusContext(model);
		
	}else if(e.getActionCommand().equals("deleteAll")){
		
		int asw=JOptionPane.showConfirmDialog(BiochamMainFrame.frame,"Are you sure you want to delete all the model reactions?","Confirm",JOptionPane.YES_NO_OPTION);
		if(asw==JOptionPane.YES_OPTION){
			/*for(int i=0;i<rules.size();i++){
				model.sendToBiocham("delete_rules({"+rules.get(i).getName()+","+rules.get(i).getValue()+"}).\n");
			}*/
			model.sendToBiocham("clear_rules.\n");
			model.sendToBiocham("list_molecules.\n");
			rules.clear();
			
			getPanel().removeAll();	
			
			ArrayList cs=new ArrayList(0);
			setPanel(refreshPanel(cs));			
			cs.clear();
			cs=null;	
		}
	}
	
}

public void addParameter() {
	
	model.getGraphEditor().getGraph().setAllAdded(false);
	setNewAdded(true);
	setSavedResponses(-1);
	element=model.getRules();
	DialogAddSpecification adds=new DialogAddSpecification(BiochamMainFrame.frame, element, "Rules Operators",getPanel());
	String value=adds.getFormula();
	if(value!=null){
		if(value.endsWith(".")){
			value=value.substring(0,value.lastIndexOf("."));
		}
		if(value!=null){
			sendRuleToBiocham(value);	    
		}
	}
	
}

/**
 * @param value
 */
public void sendRuleToBiocham(String value) {
	ArrayList<Integer> al=new ArrayList<Integer>();
	for(int k=0;k<rules.size();k++){
		if(rules.get(k).getName().equals(value)){
			al.add(k);
		}
	}
	for(int i=0;i<al.size();i++){
		rules.removeElementAt(al.get(i));
	}
	al.clear();
	al=null;
	String s="add_rules("+value+").\n";
	s+="list_molecules.\n";
	model.sendToBiocham(s,"rules");	
	s=null;
}

public HashMap<Integer,String> getParentRules() {
	
	return parentRules;
}

public boolean isNewAdded() {
	return newAdded;
}

public void setNewAdded(boolean newAdded) {
	this.newAdded = newAdded;
}

public boolean isIgnoreUndefinedParametersWarnings() {
	return ignoreUndefinedParametersWarnings;
}

public void setIgnoreUndefinedParametersWarnings(boolean i) {
	this.ignoreUndefinedParametersWarnings = i;
}

public boolean isAddRule() {
	return addRule;
}

public void setAddRule(boolean addRule) {
	this.addRule = addRule;
}

public boolean isFromGraph() {
	return fromGraph;
}

public void setFromGraph(boolean fromGraph) {
	this.fromGraph = fromGraph;
}


public int getRulesCounter() {
	return rulesCounter;
}

public void setRulesCounter(int rulesCounter) {
	this.rulesCounter = rulesCounter;
}
public boolean isDontDraw() {
	
	return dontDraw;
}
public void setDontDraw(boolean dontDraw,int cnt) {
	this.counter=cnt;
	this.dontDraw = dontDraw;
}
public int getCounter() {
	return counter;
}
public void setCounter(int counter) {
	this.counter = counter;
	if(counter<=1){
		this.dontDraw=false;
	}
}
public void setPanel(JPanel panel) {
	this.panel = panel;
}
public JPanel getPanel() {
	return panel;
}
}
