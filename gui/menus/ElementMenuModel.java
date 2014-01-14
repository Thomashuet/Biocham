package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.WorkbenchArea;

import java.awt.event.KeyEvent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;


public class ElementMenuModel extends ElementMenu{

	
	
	
	ColorMenu menu;
	JPopupMenu pMenu;
	ColorMenu runMenuItem,pRunMenuItem;	
	JMenuItem saveMenuItem,pSaveMenuItem;
	String modelName;
	BiochamDynamicTree tree;
	
	
	
	
	public ElementMenuModel(BiochamDynamicTree t){	
		tree=t;
		createMenu();
		createPopupMenu();
	}
	
	
	
	private void createMenu(){
			
		  
			    
		menu=BiochamMenuBar.createNewMenu("Model", 0);	
		ColorMenu m1 = BiochamMenuBar.createNewMenu("Reaction Rule Model", 0);	
		//ColorMenu m2 = BiochamMenuBar.createNewMenu("Simulations", 0);	
		
		
		JMenuItem m2 = newMenuItem("Simulations ");
		m2.setActionCommand("Simulations");
		m2.addActionListener(WorkbenchArea.biochamMenuBar);
		
		JMenuItem m3 = newMenuItem("Boolean Temporal Properties ");
		m3.setActionCommand("Boolean Temporal Properties");
		m3.addActionListener(WorkbenchArea.biochamMenuBar);
		JMenuItem m4 = newMenuItem("Numerical Temporal Properties ");
		m4.setActionCommand("Numerical Temporal Properties");
		m4.addActionListener(WorkbenchArea.biochamMenuBar);
		JMenuItem m5 = newMenuItem("Abstractions ");
		m5.setActionCommand("Abstractions");
		m5.addActionListener(WorkbenchArea.biochamMenuBar);
		JMenuItem r = newMenuItem("Rules");
		r.setActionCommand("rules");
		r.addActionListener(WorkbenchArea.biochamMenuBar);
		JMenuItem is = newMenuItem("Initial State");
		is.setActionCommand("initialState");
		is.addActionListener(WorkbenchArea.biochamMenuBar);
		JMenuItem p = newMenuItem("Parameters");
		p.setActionCommand("parameters");
		p.addActionListener(WorkbenchArea.biochamMenuBar);
		JMenuItem d = newMenuItem("Declarations");
		d.setActionCommand("declarations");
		d.addActionListener(WorkbenchArea.biochamMenuBar);
		JMenuItem i = newMenuItem("Invariants");
		i.setActionCommand("invariants");
		i.addActionListener(WorkbenchArea.biochamMenuBar);
		JMenuItem e = newMenuItem("Events");
		e.setActionCommand("events");
		e.addActionListener(WorkbenchArea.biochamMenuBar);
		JMenuItem m = newMenuItem("Macros");
		m.setActionCommand("macros");
		m.addActionListener(WorkbenchArea.biochamMenuBar);
		JMenuItem v = newMenuItem("Volumes");
		v.setActionCommand("volumes");
		v.addActionListener(WorkbenchArea.biochamMenuBar);
		JMenuItem mols = newMenuItem("Molecules");
		mols.setActionCommand("molecules");
		mols.addActionListener(WorkbenchArea.biochamMenuBar);
		
		/*JMenuItem ode = newMenuItem("ODE");
		ode.setActionCommand("ode");
		ode.addActionListener(tree.treeListener);
		JMenuItem stochastic = newMenuItem("Stochastic");
		stochastic.setActionCommand("stochastic");
		stochastic.addActionListener(tree.treeListener);
		JMenuItem bool = newMenuItem("Boolean");
		bool.setActionCommand("bool");
		bool.addActionListener(tree.treeListener);
		m2.add(ode);
		m2.add(stochastic);
		m2.add(bool);*/
		
	    JMenuItem exportMenuItem = newMenuItem("Export Model As");//,Icons.icons.get("saveas16.png"+1.35));	 
	    exportMenuItem.setActionCommand("exportModelAs");
	    exportMenuItem.addActionListener(tree.treeListener);		    
	    JMenuItem addMenuItem = newMenuItem("Add To Model");//,Icons.icons.get("SSicon_Add2.png"+0.5)); 
	    addMenuItem.setActionCommand("addToModel");
	    addMenuItem.addActionListener(tree.treeListener);	    
	    JMenuItem closeMenuItem = newMenuItem("Close the Model");//,Icons.icons.get("fileclose-32.png"+0.7));
	    closeMenuItem.setActionCommand("closeModel");
	    closeMenuItem.addActionListener(tree.treeListener);
	  
	    m1.add(r);
	    m1.add(is);
	    m1.add(p);
	    m1.add(d);
	    m1.add(i);
	    m1.add(e);
	    m1.add(m);
	    m1.add(v);
	    m1.add(mols);
	    menu.add(m1);
	    
    	menu.add(m2);	
    	menu.add(m3);
    	menu.add(m4);
     	menu.add(m5);     	
    	menu.addSeparator();
	    menu.add(exportMenuItem);
    	menu.add(addMenuItem);	
    	menu.add(closeMenuItem);	    	
    	
    	this.setMenu(menu);
	}
	
	
	private void createPopupMenu(){
	
		
		pMenu=BiochamMenuBar.createNewPopupMenu();
				
		JMenuItem exportMenuItem = newMenuItem("Export Model As");	    
		exportMenuItem.setMnemonic(KeyEvent.VK_E);		
	    
	    exportMenuItem.addActionListener(tree.treeListener);	 	    
	    JMenuItem addMenuItem = newMenuItem("Add To Model");    
	    addMenuItem.setActionCommand("addToModel");
	   
	    addMenuItem.addActionListener(tree.treeListener);	    
	    JMenuItem closeMenuItem = newMenuItem("Close the Model");   
	    closeMenuItem.setMnemonic(KeyEvent.VK_C);		
	   
	    closeMenuItem.addActionListener(tree.treeListener);
	    
	    pMenu.add(exportMenuItem);			    
		pMenu.addSeparator();
		pMenu.add(addMenuItem);	    
    	pMenu.addSeparator();
    	pMenu.add(closeMenuItem);	
    	
		this.setPMenu(pMenu);
		
	}
	
	
	@Override
	public void setMenu(ColorMenu m) {
		menu=m;		
	}
	@Override
	public void setPMenu(JPopupMenu m) {
		pMenu=m;		
	}
	
	public ColorMenu getMenu(){
		return menu;
	}
	public JPopupMenu getPMenu(){
		return pMenu;
	}

	@Override
	public ColorMenu refreshed(BiochamModel m) {
		modelName=m.getModelName();
		this.menu.setName(modelName);
		this.menu.setText(modelName);
		setTitle(modelName);
		return menu;
	}

	public String getModelName() {
		return modelName;
	}

	public void setModelName(String modelName) {
		this.modelName = modelName;
		this.menu.setName(modelName);
		this.menu.setText(modelName);
		this.menu.revalidate();
		setTitle(modelName);
	}
	
	private void setTitle(String s){
		this.menu.setName(s);
		this.menu.setText(s);
	}
	@Override
	public JPopupMenu refreshedPopup(BiochamModel m) {
		modelName=m.getModelName();
		this.pMenu.setName(modelName);		
		setTitle(modelName);		
		return pMenu;
	}
}
