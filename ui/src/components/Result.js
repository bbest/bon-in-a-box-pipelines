import { useState } from "react";
import RenderedMap from './RenderedMap';
import React, { useRef, useEffect, useContext } from 'react';
import RenderedCSV from './csv/RenderedCSV';

const RenderContext = React.createContext();

export function Result(props) {
    const [activeRenderer, setActiveRenderer] = useState([]);

    function toggleVisibility(componentId) {
        setActiveRenderer(activeRenderer === componentId ? null : componentId);
    }

    if (props.data || props.logs) {
        return (
            <div>
                <RenderContext.Provider value={{ active: activeRenderer, toggleVisibility: toggleVisibility }}>
                    <RenderedFiles key="files" files={props.data} metadata={props.metadata} />
                    <RenderedLogs key="logs" logs={props.logs} />
                </RenderContext.Provider>
            </div>
        );
    }

    return null;
}

function isRelativeLink(value) {
    if (typeof value.startsWith === "function") { 
        return value.startsWith('/')
    }
    return false
}

function FoldableOutput(props) {
    const renderContext = useContext(RenderContext);
    let active = renderContext.active === props.componentId;
    const titleRef = useRef(null);

    useEffect(() => {
        if (active) {
            titleRef.current.scrollIntoView({ block: 'start', behavior: 'smooth' });
        }
    }, [active]);

    return <>
        <div className="outputTitle">
            <h3 ref={titleRef} onClick={() => renderContext.toggleVisibility(props.componentId)} className="clickable">
                {active ? <b>–</b> : <b>+</b>} {props.title}
            </h3>
            {props.inline && (
                isRelativeLink(props.inline) ? (
                    active && props.inline && <a href={props.inline} target="_blank" rel="noreferrer">{props.inline}</a>
                ) : (
                    !active && props.inline
                )
            )}

        </div>
        {active &&
            <div className="outputContent">
                {props.description && <p className="outputDescription">{props.description}</p>}
                {props.children}
            </div>}
    </>;
}

function RenderedFiles(props) {
    const metadata = props.metadata;

    function getMimeType(key) {
        if (metadata
            && metadata.outputs
            && metadata.outputs[key]
            && metadata.outputs[key].type) {
            return metadata.outputs[key].type;
        }
        return "unknown";
    }

    function renderWithMime(key, content) {
        let [type, subtype] = getMimeType(key).split('/');
        switch (type) {
            case "image":
                // Match many MIME type possibilities for geotiffs
                // Official IANA format: image/tiff; application=geotiff
                // Others out there: image/geotiff, image/tiff;subtype=geotiff, image/geo+tiff
                // See https://github.com/opengeospatial/geotiff/issues/34
                // Plus covering a common typo when second F omitted
                if (subtype && subtype.includes("tif") && subtype.includes("geo")) {
                    return <RenderedMap tiff={content} />;
                }
                return <img src={content} alt={key} />;

            case "text":
                if (subtype === "csv")
                    return <RenderedCSV url={content} delimiter="," />;
                if (subtype === "tab-separated-values")
                    return <RenderedCSV url={content} delimiter="&#9;" />;
                else
                    return <p>{content}</p>;

            case "unknown":
                return <>
                    <p className="error">Missing mime type in output description</p>
                    {// Fallback code to render the best we can. This can be useful if temporary outputs are added when debugging a script.
                        isRelativeLink(content) ? (
                            // Match for tiff, TIFF, tif or TIF extensions
                            content.search(/.tiff?$/i) !== -1 ? (
                                <RenderedMap tiff={content} />
                            ) : (
                                <img src={content} alt={key} />
                            )
                        ) : ( // Plain text or numeric value
                            <p>{content}</p>
                        )}
                </>;

            default:
                return <p>{content}</p>;
        }
    }

    if (props.files) {
        return Object.entries(props.files).map(entry => {
            const [key, value] = entry;

            if (key === "warning" || key === "error") {
                return value && <p key={key} className={key}>{value}</p>;
            }

            let title = key;
            let description = null;
            if (metadata
                && metadata.outputs
                && metadata.outputs[title]) {
                let output = metadata.outputs[title];
                if (output.label)
                    title = output.label;

                if (output.description)
                    description = output.description;
            }

            return (
                <FoldableOutput key={key} title={title} description={description} componentId={key} inline={value}>
                    {renderWithMime(key, value)}
                </FoldableOutput>
            );
        });
    } else {
        return null;
    }
}

function RenderedLogs(props) {
    const myId = "logs";

    if (props.logs) {
        return (
            <FoldableOutput title="Logs" componentId={myId}>
                <pre>{props.logs}</pre>
            </FoldableOutput>
        );
    }
    return null;
}
