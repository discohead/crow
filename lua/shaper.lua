local Shaper = {}

function Shaper.new()
  local s = {
    _mode = 'tones', -- options: 'ji', 'abs', 'interp', 'functions', 'map'
    weighting = 'neighbor', -- options: 'global'
    period = {input = 1, output = 1}, -- sets folding point for input and output; should these be renamed?
    phase = 0,
    folds = 0,
    window = 0,
    luv = 0, -- lookup-value found in table
    value = 0, -- luv converted to output value
    map = function(x) return x end, -- for custom mappings of LUV to value
    _weights = {},
    _normalWeights = {}, -- weights normalized to sum to 1
    _boundaries = {}, -- window locations from [0-1] in phase space
    _scale = {cycle = 12, lut = {0,2,4,5,7,9,11}} -- cycle sets foldover value
  }
  setmetatable(s,Shaper)
  s.weights = 'equal'
  return s
end

function Shaper.init(table)
  local shaper = Shaper.new()
  shaper(table)
  return shaper
end

function Shaper:determinePhase(cv)
  self.folds = math.floor(cv/self.period.input)
  self.phase = cv/self.period.input - self.folds
end

function Shaper:determineBoundaries()
  -- boundaries separate [0-1] into n regions
  -- last boundary should always be 1
  if self.weighting == 'global' then
    -- normalized weights set boundaries
    -- if one weight increases, its normalized weight will steal probability mass
    -- from all of the other _weights so that window becomes larger while
    -- all the others become smaller.
    local accum = 0
    for i=1,#self._normalWeights do
      accum = accum + self._normalWeights[i]
      self._boundaries[i] = accum
    end
  elseif self.weighting == 'neighbor' then
    -- boundaries are initially distributed according to
    -- the function which maps the LUT to output value or evenly distributed
    -- when one weight increases, its window expands and compresses the adjacent windows, but doesn't affect other windows
    local scale = self._scale.lut
    local last = self._scale.cycle
    local weights = self._weights
    local getLoc = function() end
    if self._mode == 'tones' then
      getLoc = function(ix) return scale[ix]/last end
    elseif self._mode == 'ji' then
      getLoc = function(ix) return math.log(scale[ix])/math.log(last) end
    elseif self._mode == 'map' then
      getLoc = function(ix) return map(scale[ix])/map(last) end
    else
      -- other modes just have lookup values linearly dispersed (pre weight-adjustment)
      getLoc = function(ix) return (ix-1)/#scale end
    end
    -- interpolate window between two locations according to each boundaries weight
    for i=1,#scale-1 do
      self._boundaries[i] = getLoc(i) + (getLoc(i+1)-getLoc(i))*weights[i]/(weights[i]+weights[i+1])
    end
    self._boundaries[#scale] = getLoc(#scale)+(1-getLoc(#scale))*weights[#scale]/(weights[#scale]+weights[1])
  end
end

function Shaper:determineWindow(_phase)
  local phase = _phase or self.phase
  self.window = 1
  while (phase > self._boundaries[self.window]) and (self.window <= #self._boundaries) do
    self.window = self.window+1
  end
  -- if window has increased above final boundary, we must fold.
  if self.window > #self._boundaries then
    self.window = 1
    self.folds = self.folds+1
  end
end

function Shaper:mapWindowToValue()
  local window = self.window
  local mode   = self._mode
  local scale  = self._scale
  self.luv = scale.lut[window] -- get lookup value for determined window
  if mode == 'tones' then
    -- n-TET; _scale.cycle sets tones per fold
    self.value = self.luv/scale.cycle + self.folds;
    self.value = self.value * self.period.output
  elseif mode == 'ji' then
    -- just intonation; _scale.cycle changes exponential base for folds
    self.value = math.log(self.luv)/math.log(scale.cycle) + self.folds
    self.value = self.value * self.period.output
  elseif mode == 'abs' then
    -- Absoute lookup with no interpolation and no output scaling
    self.value = self.luv + self.folds*scale.cycle
  elseif mode == 'interp' then
    -- interpolate between values
    -- nb: this is affected by weights, since weights determine boundaries!
    -- this means that you can use the weights to deform a LUT.
    -- self.period.output becomes amplitude control LUT
    -- self.period.input will control "speed" through LUT - input amplitude must increase though to complete traversal
    local lower = self.phase - (self._boundaries[window-1] or 0)-- if window = 1, lower boundary is 0
    local upper = self._boundaries[window]-self.phase
    local boundaryDiff = upper + lower -- x-distance between two boundaries
    local phase = lower/boundaryDiff -- phase location within two boundaries
    local upperDest = scale.lut[window+1] or scale.cycle -- if window = #scale, upper value is loop point
    local distance = upperDest - self.luv -- y distance between two boundaries
    self.value = self.luv + phase * distance -- interpolate between two lookup values.
    self.value = self.value + self.folds*scale.cycle -- add in octaves
    self.value = self.value * self.period.output
  elseif mode ==  'functions' then
    for i=1,self.folds do
      scale.cycle() -- should these be executed?
    end
    self.value = self.luv
    self.value() -- should these be executed?
  elseif mode == 'map' then
    self.value = self.map(self.luv)+self.map(self.folds*scale.cycle)
    self.value = self.value * self.period.output
  end
end


function Shaper:process(cv)
  self:determinePhase(cv)
  self:normalizeWeights()
  self:determineBoundaries()
  self:determineWindow()
  self:mapWindowToValue()
  return self.value
end

function Shaper:normalizeWeights(_weights)
  self._weights = _weights or self._weights
  local accum = 0
  for i=1,#self._scale.lut do
    accum = accum+self._weights[i]
  end
  self._normalWeights = {}
  for i=1,#self._scale.lut do
    self._normalWeights[i]=self._weights[i]/accum
  end
end

function Shaper:setWeights(v)
  if v == 'equal' then
    self._weights = {}
    for i =1,#self._scale.lut do
      self._weights[i] = 1
    end
  else
    if #v == #self._scale.lut then self._weights = v
  end
  self:normalizeWeights()
end

function Shaper:setLUT(lut)
  self._scale.lut = {}
  if     lut == 'major' then
    self._mode = 'tones'
    self._scale.lut = {0,2,4,5,7,9,11}
    self._scale.cycle = 12
  elseif lut == 'minor' then
    self._mode = 'tones'
    self._scale.lut = {0,2,3,5,7,8,10}
    self._scale.cycle = 12
  elseif lut == 'log' then
    self._mode = 'interp'
    -- todo
  elseif lut == 'exp' then
    self._mode = 'interp'
    -- todo
  elseif lut == 'sine' then
    self._mode = 'interp'
    for i = 1,64 do
      self._scale.lut[i] = math.sin(2*math.pi*(i-1)/64)
      self._scale.cycle = 0
    end
  elseif lut == 'cosine' then
    self._mode = 'interp'
    for i = 1,64 do
      self._scale.lut[i] = math.cos(2*math.pi*(i-1)/64)
      self._scale.cycle = 0
    end
  else
    self._scale.lut = lut
  end
end

function Shaper:setMode(mode)
  self._mode = mode
  self.weighting = 'neighbor'
  if     mode == 'ji' then
    self._scale.cycle = 2  -- default LUT fold at 2
  elseif mode == 'tones' then
    self._scale.cycle = 12 -- default LUT fold at 12
  elseif mode == 'abs' then
    self.weighting = 'global'
    self._scale.cycle = 1  -- default LUT fold at 0
  elseif mode == 'interp' then
    self._scale.cycle = 0  -- default LUT fold at 0
  elseif mode == 'functions' then
    self.weighting = 'global'
    self._scale.cycle = function() end
  elseif mode == 'sine' then
    self.scale = mode -- uses __newindex to call setLUT() (not actual modes, but useful shortcuts)
  elseif mode == 'cosine' then
    self.scale = mode
  elseif mode == 'major' then
    self.scale = mode
  elseif mode == 'minor' then
    self.scale = mode
  end
end

Shaper.__index = function(self,ix)
  if     ix == 'determinePhase' then return Shaper.determinePhase
  elseif ix == 'determineBoundaries' then return Shaper.determineBoundaries
  elseif ix == 'determineWindow' then return Shaper.determineWindow
  elseif ix == 'mapWindowToValue' then return Shaper.mapWindowToValue
  elseif ix == 'normalizeWeights' then return Shaper.normalizeWeights
  elseif ix == 'setWeights' then return Shaper.setWeights
  elseif ix == 'setLUT' then return Shaper.setLUT
  elseif ix == 'setMode' then return Shaper.setMode
  elseif ix == 'process' then return Shaper.process
  elseif ix == 'scale' or ix == 'lut' then return self._scale.lut
  elseif ix == 'octave' or ix == 'cycle' then return self._scale.cycle
  elseif ix == 'weights' then return self._weights
  end
end

Shaper.__newindex = function(self,ix,val)
  if ix == 'octave' or ix == 'cycle' then
    self._scale.cycle = val
  elseif ix == 'weights' then
    -- todo: deal with mismatch between weights/scale length
      self:setWeights(val)
  elseif ix == 'scale' or ix == 'lut' then
    -- todo: deal with mismatch between weights/scale length
    self:setLUT(val)
  elseif ix == 'mode' then
    self:setMode(val)
  end
end

Shaper.__call = function(self,x)
  -- todo: test
  if type(x) == 'number' then
    self:process(x)
  elseif type(x) == 'table' then
    for k,v in pairs(x) do
      if     k == 'mode' then self:setMode(v)
      elseif k == 'weights' then self:setWeights(v)
      elseif k == 'scale' or k == 'lut' then self:setLUT(v)
      elseif k == 'octave' or k == 'cycle' then self._scale.cycle = v
      elseif k == 'period' then
          self.period.input = v
          self.period.output = v
      elseif k=='weighting' then
        self[k]=v
      end
    end
  end
end

setmetatable(Shaper,Shaper) -- capture metamethods

return Shaper
