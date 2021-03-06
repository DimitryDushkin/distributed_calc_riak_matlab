%% HRL file generated by ERLSOM
%%
%% It is possible to change the name of the record fields.
%%
%% It is possible to add default values, but be aware that these will
%% only be used when *writing* an xml document.

-record('p:GetDataSetValues', {anyAttribs, 'dataSetName'}).
-record('p:GetDataSetValuesByRange', {anyAttribs, 'from', 'to'}).
-record('p:GetDataSetValuesByRangeResponse', {anyAttribs, 'values'}).
-record('p:GetDataSetValuesResponse', {anyAttribs, 'values'}).
-record('p:GetDataSetsList', {anyAttribs, 'in'}).
-record('p:GetDataSetsListResponse', {anyAttribs, 'dataSetsList'}).
-record('soap:Body', {anyAttribs, choice}).
-record('soap:Envelope', {anyAttribs, 'Header', 'Body', choice}).
-record('soap:Fault', {anyAttribs, 'faultcode', 'faultstring', 'faultactor', 'detail'}).
-record('soap:Header', {anyAttribs, choice}).
-record('soap:detail', {anyAttribs, choice}).
