import 'react-flow-renderer/dist/style.css';
import 'react-flow-renderer/dist/theme-default.css';
import './Editor.css';

import React, { useState, useRef, useCallback, useEffect } from 'react';
import ReactFlow, {
  ReactFlowProvider,
  addEdge,
  useNodesState,
  useEdgesState,
  getConnectedEdges,
  Controls,
  MiniMap,
  Position,
} from 'react-flow-renderer/nocss';

import IONode from './IONode'
import ConstantNode, { ARRAY_PLACEHOLDER } from './ConstantNode'
import { getLayoutedElements } from './react-flow-utils/Layout'
import { highlightConnectedEdges } from './react-flow-utils/HighlightConnectedEdges'
import { getUpstreamNodes, getDownstreamNodes } from './react-flow-utils/getConnectedNodes'
import { getScriptDescription } from './ScriptDescriptionStore'

const customNodeTypes = {
  io: IONode,
  constant: ConstantNode
}

let id = 0;
const getId = () => `${id++}`;

/**
 * @returns rendered view of the pipeline inputs
 */
const IOList = ({inputList, outputList, selectedNodes}) => {
  function listInputs(metadata, inputs){
    return inputs.map((input, i)=> {
      return <p key={i}>
        {metadata.inputs[input].label ? metadata.inputs[input].label : input}<br />
        <span className='description'>{metadata.inputs[input].description}</span>
      </p>
    })
  }

  return <div className='ioList'>
    <h3>User inputs</h3>
    {inputList.length === 0 ? "No inputs" : inputList.map((script, i) => {
      return <div key={i} className={selectedNodes.find(node => node.id === script.id) ? "selected" : ""}>
        {listInputs(getScriptDescription(script.file), script.missing)}
      </div>
    })}
    <h3>Pipeline outputs</h3>
    {outputList.length === 0 ?
      <p className='error'>At least one output is needed for the pipeline to run</p>
      : outputList.map((output, i) => {
        return <div key={i} className={selectedNodes.find(node => node.id === output.nodeId) ? "selected" : ""}>
          <p>{output.label}<br />
            <span className='description'>{output.description}</span>
          </p>
        </div>
      })}
  </div>
}

export function PipelineEditor(props) {
  const reactFlowWrapper = useRef(null);
  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);
  const [reactFlowInstance, setReactFlowInstance] = useState(null);
  const [selectedNodes, setSelectedNodes] = useState(null);
  const [inputList, setInputList] = useState([])
  const [outputList, setOutputList] = useState([])

  const [toolTip, setToolTip] = useState(null);

  // We need this since the functions passed through node data retain their old selectedNodes state.
  // Note that the stratagem fails if trying to add edges from many sources at the same time.
  const [pendingEdgeParams, addEdgeWithHighlight] = useState(null)
  useEffect(()=>{
    if(pendingEdgeParams) {
      setEdges((edgesList) => highlightConnectedEdges(selectedNodes, addEdge(pendingEdgeParams, edgesList)))
      addEdgeWithHighlight(null)
    }
  }, [pendingEdgeParams, selectedNodes, setEdges])

  const inputFile = useRef(null) 

  const onConnect = useCallback((params) => {
    setEdges((edgesList) =>  
      highlightConnectedEdges(selectedNodes, addEdge(params, edgesList))
    )
  }, [selectedNodes, setEdges]);

  const onDragOver = useCallback((event) => {
    event.preventDefault();
    event.dataTransfer.dropEffect = 'move';
  }, []);

  const onConstantValueChange = useCallback((event) => {
    setNodes((nds) =>
      nds.map((node) => {
        if(node.id !== event.target.id) {
          return node
        }

        let value
        if(event.target.type === 'checkbox') {
          value = event.target.checked
        } else {
          value = event.target.value

          if(event.target.placeholder === ARRAY_PLACEHOLDER) {
            value = value.split(',').map(v => v.trim())
          }
        }

        return {
          ...node,
          data: {
            ...node.data,
            value,
          },
        };
      })
    );
  }, [setNodes]);

  const onSelectionChange = useCallback((selected) => {
    setSelectedNodes(selected.nodes)
    setEdges(highlightConnectedEdges(selected.nodes, edges))
  }, [edges, setEdges, setSelectedNodes])

  const injectConstant = useCallback((event, fieldDescription, target, targetHandle) => {
    event.preventDefault()

    const reactFlowBounds = reactFlowWrapper.current.getBoundingClientRect()

    // Offset from pointer event to canvas
    const position = reactFlowInstance.project({
      x: event.clientX - reactFlowBounds.left,
      y: event.clientY - reactFlowBounds.top,
    })

    // Approx offset so the node appears near the input.
    position.x = position.x - 350
    position.y = position.y - 15

    const newNode = {
      id: getId(),
      type: 'constant',
      position,
      dragHandle: '.dragHandle',
      data: {
        onChange: onConstantValueChange,
        type: fieldDescription.type,
        value: fieldDescription.example,
        options: fieldDescription.options,
      },
    }
    setNodes((nds) => nds.concat(newNode))

    const newEdge = {
      source: newNode.id,
      sourceHandle: null,
      target: target,
      targetHandle: targetHandle
    }
    addEdgeWithHighlight(newEdge)
  }, [reactFlowInstance, onConstantValueChange, setNodes])

  const injectOutput = useCallback((event, source, sourceHandle) => {
    event.preventDefault()

    const reactFlowBounds = reactFlowWrapper.current.getBoundingClientRect()

    // Offset from pointer event to canvas
    const position = reactFlowInstance.project({
      x: event.clientX - reactFlowBounds.left,
      y: event.clientY - reactFlowBounds.top,
    })

    // Approx offset so the node appears near the output.
    position.x = position.x + 100
    position.y = position.y - 15

    const newNode = {
      id: getId(),
      type: 'output',
      position,
      targetPosition: Position.Left,
      data: { label: 'Output' }
    };
    setNodes((nds) => nds.concat(newNode))

    const newEdge = {
      source: source,
      sourceHandle: sourceHandle,
      target: newNode.id,
      targetHandle: null
    }
    addEdgeWithHighlight(newEdge)
  }, [reactFlowInstance, setNodes])

  const onNodesDelete = useCallback((deletedNodes) => {
    // We delete constants that are connected to no other node
    const upstreamNodes = getUpstreamNodes(deletedNodes, reactFlowInstance)
    const allEdges = reactFlowInstance.getEdges()
    let toDelete = upstreamNodes.filter(n => n.type === 'constant' && getConnectedEdges([n], allEdges).length === 1)
    
    // We delete outputs that are connected to no other node
    const downstreamNodes = getDownstreamNodes(deletedNodes, reactFlowInstance)
    toDelete = toDelete.concat(downstreamNodes.filter(n => n.type === 'output' && getConnectedEdges([n], allEdges).length === 1))

    //version 11.2 will allow reactFlowInstance.deleteElements(toDelete)
    const deleteIds = toDelete.map(n => n.id)
    setNodes(nodes => nodes.filter(n => !deleteIds.includes(n.id)))
  }, [reactFlowInstance, setNodes])

  const onDrop = useCallback((event) => {
      event.preventDefault();

      const reactFlowBounds = reactFlowWrapper.current.getBoundingClientRect();
      const type = event.dataTransfer.getData('application/reactflow');
      const descriptionFile = event.dataTransfer.getData('descriptionFile');

      // check if the dropped element is valid
      if (typeof type === 'undefined' || !type) {
        return;
      }

      const position = reactFlowInstance.project({
        x: event.clientX - reactFlowBounds.left,
        y: event.clientY - reactFlowBounds.top,
      });

      const newNode = {
        id: getId(),
        type,
        position,
      };

      switch (type) {
        case 'io':
          newNode.data = { 
            descriptionFile: descriptionFile,
            setToolTip: setToolTip,
            injectConstant: injectConstant,
            injectOutput: injectOutput
           }
          break;
        case 'output':
          newNode.data = { label: 'Output' }
          newNode.targetPosition = Position.Left
          break;
        default:
          throw Error("unknown node type")
      }

      setNodes((nds) => nds.concat(newNode));
    },
    [reactFlowInstance, injectConstant, injectOutput, setNodes]
  )

  /**
   * Refresh the list of "dangling" inputs that the user will need to provide.
   */
  useEffect(() => {
    let newUserInputs = []
    nodes.forEach(node => {
      if (node.type === 'io' && node.data) {
        let scriptDescription = getScriptDescription(node.data.descriptionFile)
        if (scriptDescription && scriptDescription.inputs) {
          let missingInputs = []
          Object.keys(scriptDescription.inputs).forEach(inputId => {
            if (-1 === edges.findIndex(edge => edge.target === node.id && edge.targetHandle === inputId)) {
              missingInputs.push(inputId)
            }
          })

          if (missingInputs.length > 0) {
            newUserInputs.push({
              id: node.id,
              file: node.data.descriptionFile,
              missing: missingInputs
            })
          }
        }
      }
    })

    setInputList(newUserInputs)
  }, [nodes, edges, setInputList])

  /**
   * Refresh the list of outputs.
   */
  useEffect(() => {
    if(!reactFlowInstance)
      return

    let newPipelineOutputs = []
    const allNodes = reactFlowInstance.getNodes()
    allNodes.forEach(node => {
      if (node.type === 'output') {
        const connectedEdges = getConnectedEdges([node], edges)
        const edgesUpstream = connectedEdges.filter(edge => edge.target === node.id) // 0..n
        edgesUpstream.forEach(edge => {
          const sourceNode = allNodes.find(n => n.id === edge.source) // Always 1
          const stepDescription = getScriptDescription(sourceNode.data.descriptionFile)
          const outputDescription = stepDescription.outputs[edge.sourceHandle]
          newPipelineOutputs.push({
            nodeId: edge.source,
            output: edge.sourceHandle,
            file: sourceNode.data.descriptionFile,
            label: outputDescription.label,
            description: outputDescription.description
          })
        })
      }
    })

    setOutputList(previousOutputs =>
      newPipelineOutputs.map(newOutput => {
        const previousOutput = previousOutputs.find(prev =>
          prev.nodeId === newOutput.nodeId && prev.output === newOutput.output
        )
        // The label and description of previous outputs might have been modified, so we keep them as is.
        return previousOutput ? previousOutput : newOutput
      })
    )
  }, [edges, reactFlowInstance, setOutputList])

  const onLayout = useCallback(() => {
    const { nodes: layoutedNodes, edges: layoutedEdges } =
      getLayoutedElements(reactFlowInstance.getNodes(), reactFlowInstance.getEdges());

    setNodes([...layoutedNodes]);
    setEdges([...layoutedEdges]);
  }, [reactFlowInstance, setNodes, setEdges]);

  const onSave = useCallback(() => {
    if (reactFlowInstance) {
      const flow = reactFlowInstance.toObject();

      // react-flow properties that are not necessary to rebuild graph when loading
      flow.nodes.forEach(node => {
        delete node.selected
        delete node.dragging
        delete node.positionAbsolute
        delete node.width
        delete node.height

        // These we will reinject when loading
        delete node.targetPosition
        delete node.sourcePosition
      })

      // No need to save the on-the-fly styling
      flow.edges.forEach(edge => delete edge.style)

      // Save pipeline inputs
      flow.inputs = {}
      inputList.forEach(script => {
        let description = getScriptDescription(script.file)
        script.missing.forEach(missingInput => 
          flow.inputs[script.file + "@" + script.id + "." + missingInput] = description.inputs[missingInput]
        )
      })

      // Save pipeline outputs
      flow.outputs = outputList

      navigator.clipboard
        .writeText(JSON.stringify(flow, null, 2))
        .then(() => {
          alert("Pipeline content copied to clipboard.\nUse git to add the code to BON in a Box's repository.")
        })
        .catch(() => {
          alert("Error: Failed to copy content to clipboard.");
        });
    }
  }, [reactFlowInstance, inputList, outputList]);

  const onLoadFromFileBtnClick = () => inputFile.current.click() // will call onLoad

  const onLoad = (clickEvent) => {
    clickEvent.stopPropagation()
    clickEvent.preventDefault()

    var file = clickEvent.target.files[0]
    if (file) {
      var fr = new FileReader();
      fr.readAsText(file);
      fr.onload = loadEvent => {
        const flow = JSON.parse(loadEvent.target.result);
        if (flow) {
          id = 0
          flow.nodes.forEach(node => {
            // Make sure next id doesn't overlap
            id = Math.max(id, parseInt(node.id))

            // Reinjecting deleted properties
            node.targetPosition = "left"
            node.sourcePosition = "right"

            // Reinjecting functions
            switch (node.type) {
              case 'io':
                node.data.setToolTip = setToolTip
                node.data.injectConstant = injectConstant
                node.data.injectOutput = injectOutput
                break;
              case 'constant':
                node.data.onChange = onConstantValueChange
                break;
              case 'output':
                break;
              default:
               console.error("Unsupported type " + node.type)
            }
          })
          id++

          // Load the graph
          const { x = 0, y = 0, zoom = 1 } = flow.viewport;
          setNodes(flow.nodes || []);
          setEdges(flow.edges || []);
          reactFlowInstance.setViewport({ x, y, zoom });
        } else {
          console.error("Error parsing flow")
        }
      }

      // Now that it's done, reset the value of the input file.
      inputFile.current.value = ""
    }
  }



  return <div id='editorLayout'>
    <p>Need help? Check out <a href="https://github.com/GEO-BON/biab-2.0/#pipelines" target='_blank' rel='noreferrer'>the documentation</a></p>
    <div className="dndflow">
      <ReactFlowProvider>
        <div className="reactflow-wrapper" ref={reactFlowWrapper}>
          <ReactFlow
            nodes={nodes}
            edges={edges}
            nodeTypes={customNodeTypes}
            onNodesChange={onNodesChange}
            onEdgesChange={onEdgesChange}
            onConnect={onConnect}
            onInit={setReactFlowInstance}
            onDrop={onDrop}
            onDragOver={onDragOver}
            onSelectionChange={onSelectionChange}
            onNodesDelete={onNodesDelete}
            deleteKeyCode='Delete'
          >
            {toolTip && <div className="tooltip">
              {toolTip}
            </div>}
            
            <div className="save__controls">
              <button onClick={() => onLayout()}>Layout</button>
              <input type='file' id='file' ref={inputFile} accept="application/json"
                onChange={onLoad} style={{ display: 'none' }} />
              <button onClick={onLoadFromFileBtnClick}>Load from file</button>
              <button disabled={true}>Load from server</button>
              <button onClick={onSave}>Save</button>
            </div>

            <Controls />

            <IOList inputList={inputList} outputList={outputList} selectedNodes={selectedNodes} />

            <MiniMap
              nodeStrokeColor={(n) => {
                if (n.type === 'constant') return '#0041d0';
                if (n.type === 'output') return '#ff0072';
                return 'black'
              }}
              nodeColor={(n) => {
                if (n.type === 'constant') return '#0041d0';
                if (n.type === 'output') return '#ff0072';
                return '#ffffff';
              }}
            />
          </ReactFlow>
        </div>
      </ReactFlowProvider>
    </div>
  </div>
};
