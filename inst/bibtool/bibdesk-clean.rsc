% Clean up BibDesk bib files

% Keep cite keys the same
preserve.keys = On
preserve.key.case = On

% New entries to work with biblatex
% (bibtool complains about nonstandard entries)
new.entry.type = {online}
new.entry.type = {electronic}

% Unused fields
% Annote fields in particular cause trouble for bibtex::read.bibtex
delete.field{Annote}
delete.field{read}
delete.field{date-added}
delete.field{date-modified}
delete.field{bdsk-file-1}
delete.field{bdsk-file-2}
delete.field{bdsk-file-3}
delete.field{bdsk-file-4}
delete.field{bdsk-file-5}
delete.field{bdsk-url-1}
delete.field{bdsk-url-2}
delete.field{bdsk-url-3}
delete.field{bdsk-url-4}
delete.field{bdsk-url-5}
