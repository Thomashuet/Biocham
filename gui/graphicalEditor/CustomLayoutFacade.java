package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.CellView;
import org.jgraph.graph.DefaultEdge;

import com.jgraph.layout.JGraphFacade;







public class CustomLayoutFacade extends JGraphFacade{

	
	BiochamGraph graph;
	
	public CustomLayoutFacade(BiochamGraph graphic,Object[] roots ) {
		super(graphic,roots);
		//resetControlPoints();
		setDirected(true);
		setIgnoresCellsInGroups(true);
		setOrdered(true);		
		resetControlPoints(true,BiochamGraphConstants.ROUTING_SIMPLE);
		graph=graphic;		
		this.determineLayoutHierarchies();
		this.findTreeRoots();
	}
	
	public CustomLayoutFacade(BiochamGraph graphic) {
		super(graphic);		
		graph=graphic;
		//super.setIgnoresCellsInGroups(true);
		//super.setIgnoresHiddenCells(true);
		//super.setOrdered(true);
		
	}

	
	
	public boolean isVertex(Object cell) {
		
	
		if (verticesFilter != null) {
			if (!verticesFilter.contains(cell)) {
				return false;
			}
		}
		// If we're dealing with an edge or a port we
		// return false in all cases
		if (BiochamGraphModel.isValidVertex( cell)) {
			if (ignoresUnconnectedCells) {
				Object[] edges = getEdges(cell);
				if (edges == null || edges.length == 0)
					return false;
				else {
					if (ignoresHiddenCells && graphLayoutCache != null) {

						// Check if at least one edge is visible
						boolean connected = false;
						for (int i = 0; i < edges.length; i++) {
							connected = connected
							|| graphLayoutCache.isVisible(edges[i]);
						}
						if (!connected)
							return false;
					}
				}
			}
			if (ignoresHiddenCells && graphLayoutCache != null) {

				// If only visible cells should be returned
				// we check if there is a cell view for the cell
				// and return if based on it's isLeaf property.
				CellView view = graphLayoutCache.getMapping(cell, false);
				if (view != null) {
					// Root cell views have no parent view
					ignoresCellsInGroups=true;
					if (ignoresCellsInGroups) {
						return (view.getParentView() == null);
					} else {
						return true;
					}
				}
				return false;
			} 
			ignoresCellsInGroups=true;
			if (ignoresCellsInGroups && model.getParent(cell) != null) {
				return false;
			}
			return true;
		}
		return false;
	}

	/**
	 * Returns whether or not the specified cell is an edge and should be taken
	 * into account by the layout
	 * 
	 * @param cell
	 *            the cell that is to be classified as an edge or not
	 * @return Returns true if the cell is an edge
	 */
	public boolean isEdge(Object cell) {
		// Hint: "Edge groups" need special attention
		// Unconnected edges are ignored
	
	
		if (ignoresHiddenCells && graphLayoutCache != null) {
			if (!BiochamGraphModel.isValidEdge(cell)) {
				return false;
			}
			CellView view = graphLayoutCache.getMapping(cell, false);
			if (view != null) {
				if (ignoresCellsInGroups) {
					return view.isLeaf() && view.getParentView() == null;
				} else {
					return view.isLeaf();
				}
			}
			return false;
		} else {
			// Returns false if we find a child that is not a port
			if (ignoresCellsInGroups && model.getParent(cell) != null) {
				return false;
			}
			return BiochamGraphModel.isValidEdge(cell);
		}
	}
	
	
	
	
	
	
	
	
}
