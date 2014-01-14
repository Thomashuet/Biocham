package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.modelData.ParamTableParameters;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

public class ElementMenuAbstractions extends ElementMenu{

	
	ColorMenu menu;
	JPopupMenu pMenu;
	ColorMenu runMenuItem,pRunMenuItem;	
	JMenuItem dim1,dim2,r1,r11,r2,r22,r3,r33,r333,r3333,pf,igr1,igr2,igr3,ngr1,ngr2,ngr3;
	JMenuItem saveMenuItem,pSaveMenuItem;
	String modelName;
	BiochamDynamicTree tree;
	
	public ElementMenuAbstractions(BiochamDynamicTree t){	
		tree=t;
		createMenu();
		createPopupMenu();
	}
	
	
	
	private void createMenu(){
			
		
		menu=new ColorMenu("Abstractions");		
		
		
		ColorMenu reductions=new ColorMenu("Reductions");//smerge,sdelete,rmerge,rdelete,search reduction, search all reductions,search mReduction, search all mReductions.
		
				
		ColorMenu merge=new ColorMenu("Merge");		
		r1 = newMenuItem("Species"); 
		r1.setActionCommand("sMerge");
		r1.addActionListener(tree.treeListener);	
		r11 = newMenuItem("Reactions"); 
		r11.setActionCommand("rMerge");
		r11.addActionListener(tree.treeListener);			
		merge.add(r1);
		merge.addSeparator();
		merge.add(r11);
		
		ColorMenu delete=new ColorMenu("Delete");		
		r2 = newMenuItem("Species"); 
		r2.setActionCommand("sDelete");
		r2.addActionListener(tree.treeListener);				
		r22 = newMenuItem("Reactions"); 
		r22.setActionCommand("rDelete");
		r22.addActionListener(tree.treeListener);	
		delete.add(r2);
		delete.addSeparator();
		delete.add(r22);
		
		ColorMenu search=new ColorMenu("Search");		
		
		r3 = newMenuItem("Search Reduction"); 
		r3.setActionCommand("searchReduction");
		r3.addActionListener(tree.treeListener);
		r33 = newMenuItem("Search All Reductions"); 
		r33.setActionCommand("searchAllReductions");
		r33.addActionListener(tree.treeListener);
		r333 = newMenuItem("Search Merge-Only Reduction"); 
		r333.setActionCommand("searchMOReduction");
		r333.addActionListener(tree.treeListener);
		r3333 = newMenuItem("Search All Merge-Only Reduction"); 
		r3333.setActionCommand("searchAllMOReduction");
		r3333.addActionListener(tree.treeListener);
		
		reductions.add(merge);
		reductions.add(delete);
		search.add(r3);
		search.addSeparator();
		search.add(r33);
		search.addSeparator();
		search.add(r333);
		search.addSeparator();
		search.add(r3333);
		reductions.add(search);
		
		
	/*	
		ColorMenu dimensions=new ColorMenu("Dimensions");	//list,set
		
		
		dim1 = newMenuItem("List"); 
		dim1.setActionCommand("listDimensions");
	
		dim2 = newMenuItem("Set"); 
		dim2.setActionCommand("setDimension");		
		dimensions.add(dim1);	
		dimensions.addSeparator();
		dimensions.add(dim2);*/
		
		ColorMenu inflGraph=new ColorMenu("Influence Graph");	//list,draw,export
		
		
		
		/*igr1 = newMenuItem("List"); 
		igr1.setActionCommand("listIG");
		igr1.addActionListener(tree);		*/
		igr2 = newMenuItem("Draw"); 
		igr2.setActionCommand("typing");
		igr2.addActionListener(tree.treeListener);		
		/*igr3 = newMenuItem("Export"); 
		igr3.setActionCommand("exportIG");
		igr3.addActionListener(tree);*/
		//inflGraph.add(igr1);
		//inflGraph.addSeparator();
		inflGraph.add(igr2);
		//inflGraph.addSeparator();
		//inflGraph.add(igr3);
		
		/*pf = newMenuItem("Protein Functions"); 
		pf.setActionCommand("listFunctions");
		pf.addActionListener(tree);*/
		
		ColorMenu locneighbor=new ColorMenu("Neighborhood Graph");//list,draw,export		
		
		
	//	ngr1 = newMenuItem("List"); 
	//	ngr1.setActionCommand("listNG");
	//	ngr1.addActionListener(tree);		
		ngr2 = newMenuItem("Draw"); 
		ngr2.setActionCommand("reactionGraph");
		ngr2.addActionListener(tree.treeListener);		
	//	ngr3 = newMenuItem("Export"); 
	//	ngr3.setActionCommand("exportNG");
	//	ngr3.addActionListener(tree);
	//	locneighbor.add(ngr1);
	//	locneighbor.addSeparator();
		locneighbor.add(ngr2);
	//	locneighbor.addSeparator();
	//	locneighbor.add(ngr3);
		
		
    //	menu.add(reductions);    
    //	menu.add(dimensions);    	
    	menu.add(inflGraph);      	  	
    	menu.add(locneighbor); 
    //	menu.add(pf);  
    	menu.setBackground(Utils.backgroundColor);
    	menu.revalidate();
    	menu.repaint();
    	
    	this.setMenu(menu);
	}
	
	
	private void createPopupMenu(){
	
		
		pMenu=BiochamMenuBar.createNewPopupMenu();
		JMenuItem exportMenuItem = newMenuItem("Export Model As");	    
		exportMenuItem.setMnemonic(KeyEvent.VK_E);		
	    exportMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
	    exportMenuItem.setActionCommand("exportModelAs");
	    exportMenuItem.addActionListener(tree.treeListener);	 
	    
	    JMenuItem addMenuItem = newMenuItem("Add To Model");    
	    addMenuItem.setActionCommand("addToModel");
	    addMenuItem.addActionListener(tree.treeListener);
	    JMenuItem closeMenuItem = newMenuItem("Close the Model");   
	    closeMenuItem.setMnemonic(KeyEvent.VK_C);		
	    closeMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
	    closeMenuItem.setActionCommand("closeModel");
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
		menu.setBackground(Utils.backgroundColor);
		menu.validate();
		return menu;
	}
	public JPopupMenu getPMenu(){
		return pMenu;
	}

	@Override
	public ColorMenu refreshed(BiochamModel m) {
		
		/*if(dim1.getActionListeners().length>0){
			 for(int i=0;i<dim1.getActionListeners().length;i++){
				 dim1.removeActionListener(dim1.getActionListeners()[i]);
			 }
		 }
		dim1.addActionListener((ParamTableParameters)m.getParameters().getParamTable());
		if(dim2.getActionListeners().length>0){
			 for(int i=0;i<dim2.getActionListeners().length;i++){
				 dim2.removeActionListener(dim2.getActionListeners()[i]);
			 }
		 }
		dim2.addActionListener((ParamTableParameters)m.getParameters().getParamTable());
		if(((ParamTableRules)m.getRules().getParamTable()).getRules().size()>0){
			menu.setEnabled(true);
		}else{
			menu.setEnabled(false);
		}*/
		menu.revalidate();
		return this.menu;
	}

	public String getModelName() {
		return modelName;
	}

	public void setModelName(String modelName) {
		this.modelName = modelName;
	}
	
	private void setTitle(String s){
		this.menu.setName(s);
		this.menu.setText(s);
	}



	@Override
	public JPopupMenu refreshedPopup(BiochamModel m) {
		// TODO Auto-generated method stub
		return null;
	}
}

