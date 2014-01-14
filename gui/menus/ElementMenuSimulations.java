package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;

import java.awt.Toolkit;
import java.awt.event.KeyEvent;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

public class ElementMenuSimulations extends ElementMenu{

	
	ColorMenu menu;
	JPopupMenu pMenu;
	JMenu runMenuItem,pRunMenuItem;	
	 JMenuItem comparizonWindow;
	JMenuItem saveMenuItem,pSaveMenuItem;
	String modelName;
	BiochamDynamicTree tree;
	JMenuItem execute;
	
	public ElementMenuSimulations(BiochamDynamicTree t){	
		tree=t;
		createMenu();
		createPopupMenu();
	}
	
	
	
	private void createMenu(){
			
		
		menu=BiochamMenuBar.createNewMenu("Simulations", 0);		
		
		
		execute = newMenuItem("Run");
		execute.setMnemonic(KeyEvent.VK_E);		
	    execute.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
	    execute.setActionCommand("numSim");
	    execute.addActionListener(tree.treeListener);	
	    
	    comparizonWindow = newMenuItem("Comparison Window");    
	    comparizonWindow.setActionCommand("comparisonWindow");
	    comparizonWindow.addActionListener(tree.treeListener);

    	menu.add(execute);
    	menu.addSeparator();
    	menu.add(comparizonWindow);
    			    			    
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
		return menu;
	}
	public JPopupMenu getPMenu(){
		return pMenu;
	}

	@Override
	public ColorMenu refreshed(BiochamModel m) {
		if(((ParamTableRules)m.getRules().getParamTable()).getRules().size()>0){
			execute.setEnabled(true);
		}else{
			execute.setEnabled(false);
		}
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
