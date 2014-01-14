package fr.inria.contraintes.biocham.documentation;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.customComponents.CustomPlaf.MyPlafTabbedPane;
import fr.inria.contraintes.biocham.utils.Utils;

import java.util.prefs.Preferences;

import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;


public class BiochamDocumentation extends JSplitPane {

	
	private static final String screenName="Documentation";
	public static JTabbedPane tabbedPane;
	public static BiochamDynamicTree tree;
	
	
	public BiochamDocumentation(){
		
		super();
		
		tabbedPane = new JTabbedPane();
		tabbedPane.setUI(new MyPlafTabbedPane());
		tabbedPane.setOpaque(true);
		tabbedPane.setName("tabbedPane");
		tabbedPane.setBackground(Utils.backgroundColor);
		tabbedPane.setFocusable(false);
		tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);     
		tree=new BiochamDynamicTree(this);      
		super.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		super.setLeftComponent(tree);
		super.setRightComponent(tabbedPane);
		setResizeWeight(0.1);  
		Preferences prefs = BiochamMainFrame.getPrefs();    	
		setDividerLocation(prefs.getInt("xdiv", -1));
		tree.treeListener.expandAll(true);
		
		
	}
	
	
	
	
	
	
	public String toString(){
		return screenName;
	}
	
}
