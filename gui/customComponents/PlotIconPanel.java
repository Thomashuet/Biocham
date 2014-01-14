package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.menus.ColorMenu;
import fr.inria.contraintes.biocham.menus.CustomPopupMenu;
import fr.inria.contraintes.biocham.plotting.PlotsComparizonWindowUtils;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;


/**
 * Class thats creates a custom JPanel component that holds an ImageIcon of a numerical simulation plot that has been added to the ComparizonWindow.
 * On right click there is a popup to remove the image from the comparizonWindow.
 * 
 * @author Dragana Jovanovska  
 */ 

public class PlotIconPanel extends JPanel implements ActionListener{

	ImageIcon icon;
	
	public PlotIconPanel(ImageIcon ic){
		
		super();
		icon=ic;
		setBackground(Utils.backgroundColor);
		setLayout(new BorderLayout());
		JLabel l=new JLabel(ic);
		add(l,BorderLayout.CENTER);
		
		final PlotIconPopupMenu menu=new PlotIconPopupMenu(this);
		
		addMouseListener(new MouseAdapter(){
			  public void mousePressed(MouseEvent ev) {
			        if (ev.isPopupTrigger()) {
			          menu.getPopup().show(ev.getComponent(), ev.getX(), ev.getY());
			        }
			      }
			      public void mouseReleased(MouseEvent ev) {
			        if (ev.isPopupTrigger()) {
			          menu.getPopup().show(ev.getComponent(), ev.getX(), ev.getY());
			        }
			      }
		  });
	}

	public void actionPerformed(ActionEvent e) {
		String iconName=icon.getDescription();
		if(iconName!=null && iconName!=""){
			for(int i=0;i<BiochamDynamicTree.jplots.size();i++){
				if(BiochamDynamicTree.jplots.get(i).getDescription()!=null && BiochamDynamicTree.jplots.get(i).getDescription()!=""){
					if(BiochamDynamicTree.jplots.get(i).getDescription().equals(iconName)){
						BiochamDynamicTree.jplots.remove(i);
						
						if(BiochamDynamicTree.jplots.size()>0){
							PlotsComparizonWindowUtils.updateComparisonWindow();							
						}else{
							BiochamDynamicTree.comparizonWindow.removeAll();	
							BiochamDynamicTree.comparizonWindow.repaint();
						}						
						
						break;
					}
				}
			}
		}
		
	}
}


class PlotIconPopupMenu extends CustomPopupMenu {
	
	JPopupMenu menu;
	PlotIconPanel parent;
	
	public PlotIconPopupMenu(PlotIconPanel p){
		parent=p;
	}
	
	public JPopupMenu getPopup() {
			
			menu = newPopupMenu(); 
			
			
			boolean b=SwingUtilities.isEventDispatchThread();
			
			
			JMenuItem openMenuItem = newMenuItem("Remove"); 
			openMenuItem.setActionCommand("remove");
		    openMenuItem.addActionListener(parent);
		    menu.add(openMenuItem);
	   	   
		   
			return menu;
		}

	@Override
	public ColorMenu getMenu() {
		return null;
	}

	@Override
	public JPopupMenu getPMenu() {
		return menu;
	}

	@Override
	public void setMenu(ColorMenu menu) {
		
		
	}

	@Override
	public void setPMenu(JPopupMenu menu) {
	this.menu=menu;
		
	}

	@Override
	public ColorMenu refreshed(BiochamModel m) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public JPopupMenu refreshedPopup(BiochamModel m) {
		// TODO Auto-generated method stub
		return null;
	}
}