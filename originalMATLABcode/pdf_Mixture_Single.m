function result = pdf_Mixture_Single(x,p,mu,sigma)
    
    global xDomain;
    global pseudo_uniform;
    
    pseudo_normal = normpdf(xDomain,mu,sigma).*pseudo_uniform;
    
    % AH: normalising factor for guessing distribution
    normFactor_uniform = sum(pseudo_uniform);
    % AH: normalising factor for combined distribution
    normFactor_normal = sum(pseudo_normal);
    
    if normFactor_uniform == 0
        normFactor_uniform = 10^-8;
    end
    
    if normFactor_normal == 0
        normFactor_normal = 10^-8;
    end
    
    % AH: Extract heights of guessing distribution for each bin
    % AH: I guess interp1 (linear interpolation) is used in case SPEs don't exactly match values that pseudo_Uniform has been calculated for. But it's always integers, so shouldn't it?
    uniResultTemp = interp1(xDomain, pseudo_uniform, x);
    
    % AH: Extract heights of Gaussian density at each SPE and multiply by guessing distribution height. Because the Gaussian component also is windowed by the opportunity for each serial position error. Actually, I'm not sure that makes sense. If the target position occurs very early in the stream, what is the probability of a very negative SPE? That's true that it just can't occur as often, but then what happens on those particular trials? This method assumes that it is proportionally less likely, and because it's normalized I suppose all the other errors are upscaled in comparison. But what probably actually happens is that the person reports the earliest possible SPE. But to account for that, might need to do SPE case by case where you know what the target position was for each individual trial. However, in practice the Gaussian sigma always ends up small enough that the tails at those less-frequent possible SPEs are tiny.
    normResultTemp = normpdf(x,mu,sigma).*uniResultTemp;

	%AH: Normalise, which here seems to mean assume the total probability of the data = 1. In other words, take the density heights and scale them up so that for the dataset their sum = 1
    uniResultTemp = uniResultTemp/normFactor_uniform;
    normResultTemp = normResultTemp/normFactor_normal;
    
    propNorm = p;
    propUniform = 1-p;
    
    normResult = propNorm*normResultTemp;
    uniResult = propUniform*uniResultTemp;
    
    if sum(size(normResult)==size(uniResult))==2
        tempResult = normResult+uniResult;
    else
        tempResult = normResult+uniResult';
    end
    
    %xIndex = x-min(xDomain)+1;
    %results = tempResult(xIndex);
    result = tempResult;

end